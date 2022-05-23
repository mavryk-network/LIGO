{- | The input tree from TreeSitter. Doesn't have any pointers to any data
     from actual tree the TS produced and therefore has no usage limitations.

     All datatypes here are strict.
-}

module ParseTree
  ( -- * Tree/Forest
    ParseTree(..)
  , RawInfo
  , RawTree
  , SomeRawTree(..)
  , Source(..)

    -- * Invoke the TreeSitter and get the tree it outputs
  , toParseTree

    -- * Read file contents from its source
  , pathToSrc
  )
  where

import Data.Aeson (ToJSON (..), object, (.=))
import Data.ByteString (ByteString)
import Data.Functor.Classes (Show1 (..))
import Data.Text (Text)
import Data.Text qualified as Text
import Data.Text.Encoding qualified as Text
import Data.Text.IO qualified as Text
import Data.Traversable (for)
import Control.Monad ((>=>))
import Control.Monad.IO.Class (MonadIO (..))
import Foreign.C.String (peekCString)
import Foreign.Marshal.Alloc (alloca)
import Foreign.Marshal.Array (allocaArray)
import Foreign.Ptr (Ptr, nullPtr)
import Foreign.Storable (peek, peekElemOff, poke)
import Katip (LogItem (..), PayloadSelection (AllKeys), ToObject)
import TreeSitter.Language
import TreeSitter.Node
import TreeSitter.Parser
import TreeSitter.Tree hiding (Tree)

import Duplo.Tree

import Extension
import Log (Log)
import Log qualified
import Range

foreign import ccall unsafe tree_sitter_PascaLigo  :: Ptr Language
foreign import ccall unsafe tree_sitter_ReasonLigo :: Ptr Language
foreign import ccall unsafe tree_sitter_CameLigo   :: Ptr Language
foreign import ccall unsafe tree_sitter_JsLigo     :: Ptr Language

data Source = Source
  { srcPath :: FilePath
  , srcText :: Text
  } deriving stock (Eq, Ord)

instance ToJSON Source where
  toJSON src = object ["srcPath" .= srcPath src]

deriving anyclass instance ToObject Source

instance LogItem Source where
  payloadKeys = const $ const AllKeys

instance Show Source where
  show = show . srcPath

pathToSrc :: MonadIO m => FilePath -> m Source
pathToSrc p = Source p <$> liftIO (Text.readFile p)

type RawTree = Tree '[ParseTree] RawInfo
type RawInfo = (Range, Text)

-- | The tree tree-sitter produces.
data ParseTree self = ParseTree
  { ptName     :: Text         -- ^ Name of the node.
  , ptChildren :: [self]       -- ^ Subtrees.
  , ptSource   :: ~Text        -- ^ Source of the node.
  }
  deriving stock (Functor, Foldable, Traversable)

instance Show1 ParseTree where
  liftShowsPrec lift liftList d (ParseTree name children _) = showParen (d > appPrec)
    $ showString "ParseTree "
    . showsPrec (appPrec + 1) name . showChar ' '
    . liftShowsPrec lift liftList d children
    where
      appPrec :: Int
      appPrec = 10

data SomeRawTree = SomeRawTree Lang RawTree
  deriving stock (Show)

toParseTree :: (MonadIO m, Log m) => Lang -> Source -> m SomeRawTree
toParseTree dialect (Source fp input) = Log.addNamespace "toParseTree" do
  $(Log.debug) [Log.i|Reading #{fp}|]
  let language = case dialect of
        Pascal -> tree_sitter_PascaLigo
        Caml   -> tree_sitter_CameLigo
        Reason -> tree_sitter_ReasonLigo
        Js     -> tree_sitter_JsLigo

  res <- liftIO $ SomeRawTree dialect <$> withParser language \parser -> do
    let src = Text.encodeUtf8 input
    withParseTree parser src \tree ->
      withRootNode tree (peek >=> go src)

  $(Log.debug) [Log.i|Done reading #{fp}|]
  pure res
  where
    go :: ByteString -> Node -> IO RawTree
    go src node = do
      let count = fromIntegral $ nodeChildCount node
      allocaArray count $ \children -> do
        alloca $ \tsNodePtr -> do
          poke tsNodePtr $ nodeTSNode node
          ts_node_copy_child_nodes tsNodePtr children
          nodes <- for [0.. count - 1] $ \i -> do
            node' <- peekElemOff children i
            (only -> ((r, _), tree :: ParseTree RawTree)) <- go src node'
            field <-
              if   nodeFieldName node' == nullPtr
              then return ""
              else peekCString $ nodeFieldName node'
            pure $ fastMake (r, Text.pack field) tree

          ty <- peekCString $ nodeType node

          let
            start2D  = nodeStartPoint node
            finish2D = nodeEndPoint   node
            -- An empty node indicates a missing token, for example, if we have:
            -- `function idsa (const iff : int) : int is (iff`
            -- Then tree-sitter will report:
            -- `(MISSING ")" [0, 45] - [0, 45])`
            -- But won't indicate an error on the parse tree itself. According to
            -- https://github.com/tree-sitter/tree-sitter-bash/issues/27#issuecomment-410865045
            -- we can check for this by testing whether we have an empty node.
            name     = if start2D == finish2D then "ERROR" else Text.pack ty
            range = Range
              { _rStart  =
                  ( fromIntegral $ pointRow    start2D + 1
                  , fromIntegral $ pointColumn start2D + 1
                  , fromIntegral $ nodeStartByte node
                  )
              , _rFinish =
                  ( fromIntegral $ pointRow    finish2D + 1
                  , fromIntegral $ pointColumn finish2D + 1
                  , fromIntegral $ nodeEndByte node
                  )
              , _rFile = fp
              }

          pure $ fastMake
            (range, "")
            ParseTree
              { ptName     = name
              , ptChildren = nodes
              , ptSource   = cutOut range src
              }
