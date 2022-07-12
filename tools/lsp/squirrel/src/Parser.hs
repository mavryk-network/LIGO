{-# OPTIONS_GHC -Wno-orphans #-}

module Parser
  ( Severity (..)
  , Message (..)
  , ParserM
  , LineMarkerType (..)
  , LineMarker (..)
  , UnrecognizedFieldException (..)
  , CodeSource (..)
  , Info
  , ParsedInfo

  , runParserM
  , collectTreeErrors
  , parseLineMarkerText
  , flag
  , field
  , fieldOpt
  , fields
  , emptyParsedInfo
  , fillInfo
  , withComments
  , boilerplate
  , boilerplate'
  , fallthrough
  , noMatch
  ) where

import Control.Arrow
import Control.Exception (Exception (..), throwIO)
import Control.Monad.RWS hiding (Product)
import Data.Foldable (find)
import Data.Functor
import Data.Maybe (fromJust, isJust, mapMaybe)
import Data.String.Interpolate (i)
import Data.Text (Text)
import Data.Text qualified as Text
import Language.LSP.Types qualified as J
import Text.Read (readMaybe)

import Duplo.Pretty
import Duplo.Tree

import AST.Skeleton (Error (..), SomeLIGO, getLIGO)
import ParseTree
import Product
import Range

{-
  Comment grabber has 2 buffers: 1 and 2.

  1) We collect all comments before the node and append them into buffer 1.
  2) We collect all comments after the node and put them into buffer 2.
  3) `grabComments` takes all comments from buffer 1.
  4) On leaving, move move comments from 2 to 1.
-}

-- | Reader environment for the parser.
data ParserEnv = ParserEnv
  { peNodes :: [RawTree]
  -- ^ Nodes that must yet be parsed.
  , peNodeType :: Text
  -- ^ Type of the node being parsed. Stored for better error messages.
  , peNodeRange :: Range
  -- ^ Type of the node being parsed. Stored for better error messages.
  }

runParserM :: MonadIO m => ParserM a -> m (a, [Message])
runParserM p = liftIO $ (\(a, _, errs) -> (a, errs)) <$> runRWST p initEnv ([], [])
  where
    initEnv = ParserEnv
      { peNodes = []
      , peNodeType = ""
      , peNodeRange = point 0 0
      }

data Severity
  = SeverityError
  | SeverityWarning
  deriving stock (Eq, Ord, Show)

-- | Represents some diagnostic (error, warning, etc) that may be contained
-- together with some node.
--
-- Note that this is different from @Error@, which is a node by itself, and not
-- something extra that is associated with some node.
data Message = Message
  { mMessage :: Text
  , mSeverity :: Severity
  , mRange :: Range
  } deriving stock (Eq, Ord, Show)

type ParserM = RWST ParserEnv [Message] ([Text], [Text]) IO

collectTreeErrors :: Contains Range info => SomeLIGO info -> [Message]
collectTreeErrors =
  map (\(info, Error msg _) -> Message msg SeverityError (getRange info)) . collect . getLIGO

-- | The flag of some line marker.
--
-- Note that we make the assumption that a flag may only be 1 or 2, since LIGO
-- should not have system header files or be wrapped in `extern "C"` blocks.
data LineMarkerType
  = RootFile      -- ^ No flag.
  | IncludedFile  -- ^ Flag 1.
  | ReturnToFile  -- ^ Flag 2.
  deriving stock (Eq, Show)

-- | A inclusion line marker left by running `ligo preprocess`.
--
-- Note that we assume that we may only have zero or one flag instead of zero or
-- more, since flags 1 and 2 are mutually exclusive. See 'LineMarkerType'.
--
-- See also: https://gcc.gnu.org/onlinedocs/cpp/Preprocessor-Output.html
data LineMarker = LineMarker
  { lmFile :: FilePath  -- ^ The file that was included.
  , lmFlag :: LineMarkerType  -- ^ The "parsed" flag of the line marker.
  , lmLine :: J.UInt  -- ^ The line number that should be used after the inclusion.
  , lmLoc  :: Range  -- ^ The location in the preprocessed file where the line marker was added.
  } deriving stock (Eq, Show)

parseLineMarkerText :: Text -> Maybe (FilePath, LineMarkerType, J.UInt)
parseLineMarkerText marker = do
  "#" : lineStr : fileText : flags <- Just $ Text.words marker
  line <- readMaybe $ Text.unpack lineStr
  let file = Text.unpack $ Text.init $ Text.tail fileText
  let markerType = case flags of
        -- TODO: There is an edge case when there are line markers and comments
        -- (see src/test/contracts/includer.{,m,re}ligo, so we assume for now
        -- that any extra fields after "1" or "2" are comments, and anything
        -- else is a root file (possibly with comments).
        "1" : _ -> IncludedFile
        "2" : _ -> ReturnToFile
        _       -> RootFile
  pure (file, markerType, line)

parseLineMarker :: (RawInfo, ParseTree RawTree) -> Maybe LineMarker
parseLineMarker ((range, _), ParseTree ty _ marker) = do
  guard (ty == "line_marker")
  (file, markerType, line) <- parseLineMarkerText marker
  pure $ LineMarker file markerType line range

data UnrecognizedFieldException = UnrecognizedFieldException
  { ufeFieldName :: Text
  , ufeNodeType :: Text
  , ufeNodeRange :: Range
  } deriving stock (Show)

instance Exception UnrecognizedFieldException where
  displayException UnrecognizedFieldException {ufeFieldName, ufeNodeType, ufeNodeRange} =
    [i|Cannot find field `#{ufeFieldName}` while decoding `#{ufeNodeType}` (at #{ufeNodeRange}).|]

instance Scoped (Range, Text) ParserM RawTree ParseTree where
  before _ (ParseTree _ cs _) = do
    let (comms, rest) = allComments cs
    let (comms1, _)   = allComments $ reverse rest
    modify $ first  (++ comms)
    modify $ second (++ reverse comms1)

    tell $ allErrors cs

  after _ _ = do
    modify $ \(_, y) -> (y, [])

grabComments :: ParserM [Text]
grabComments = do
  ls <- gets fst
  modify \(_, y) -> ([], y)
  return ls

allComments :: [RawTree] -> ([Text], [RawTree])
allComments = first (map getBody . filter isComment) . break isMeaningful
  where
    isMeaningful :: RawTree -> Bool
    isMeaningful = not . Text.null . snd . extract

    isComment :: RawTree -> Bool
    isComment (gist -> ParseTree ty _ _) = "comment" `Text.isSuffixOf` ty

allErrors :: [RawTree] -> [Message]
allErrors = mapMaybe extractUnnamedError
  where
    extractUnnamedError :: RawTree -> Maybe Message
    extractUnnamedError tree = case only tree of
      ((r, ""), ParseTree "ERROR" _ _)
        -> Just (Message ("Unexpected: " <> getBody tree) SeverityError r)
      _ -> Nothing

getBody :: RawTree -> Text
getBody (gist -> f) = ptSource f

flag :: Text -> ParserM Bool
flag name = fieldOpt name <&> isJust

field :: Text -> ParserM RawTree
field name =
  fieldOpt name
    >>= maybe
      (do
        ParserEnv {peNodeType, peNodeRange} <- ask
        lift $ throwIO $ UnrecognizedFieldException name peNodeType peNodeRange)
      pure

fieldOpt :: Text -> ParserM (Maybe RawTree)
fieldOpt name = find ((== name) . snd . extract) <$> asks peNodes

fields :: Text -> ParserM [RawTree]
fields name = go <$> asks peNodes
  where
    go (tree : rest)
      | (_, n) <- extract tree, n == name = tree : go rest
      | errorAtTheTop tree = tree : go rest
      | otherwise = go rest

    go _ = []

    errorAtTheTop :: RawTree -> Bool
    errorAtTheTop (match -> Just (_, ParseTree "ERROR" _ _)) = True
    errorAtTheTop _ = False

newtype CodeSource = CodeSource { unCodeSource :: Text }
  deriving newtype (Eq, Ord, Show, Pretty)

type Info = [[Text], [LineMarker], Range, CodeSource]

type ParsedInfo = PreprocessedRange ': Info

emptyParsedInfo :: Product ParsedInfo
emptyParsedInfo =
  PreprocessedRange emptyPoint :> [] :> [] :> emptyPoint :> CodeSource "" :> Nil
  where
    emptyPoint = point 0 0

instance Contains [Text] xs => Modifies (Product xs) where
  ascribe = ascribeComms . getElem

fillInfo :: Functor f => f (Product xs) -> f (Product ([Text] : Range : xs))
fillInfo = fmap \it -> [] :> point 0 0 :> it

ascribeComms :: [Text] -> Doc -> Doc
ascribeComms comms
  | null comms = id
  | otherwise  = \d ->
      block $ map pp comms ++ [d]

withComments
  :: ParserM (Product xs, a)
  -> ParserM (Product ([Text] : xs), a)
withComments act = do
  comms <- grabComments
  first (comms :>) <$> act

getMarkers :: [RawTree] -> [LineMarker]
getMarkers = mapMaybe (parseLineMarker . fromJust . match)

boilerplateImpl
  :: ParserM (f RawTree)
  -> RawInfo
  -> ParseTree RawTree
  -> ParserM (Product Info, f RawTree)
boilerplateImpl handler (r, _) (ParseTree ty cs src) =
  withComments do
    -- TODO: What is exactly the appropriate action in case something ever
    -- returns 'Nothing'? 'catMaybes'? If something goes wrong, then we will
    -- probably get unwanted behavior in 'AST.Parser'.
    let markers = getMarkers cs
    f' <- local (const $ ParserEnv cs ty r) handler
    return (markers :> r :> CodeSource src :> Nil, f')

boilerplate
  :: (Text -> ParserM (f RawTree))
  -> RawInfo
  -> ParseTree RawTree
  -> ParserM (Product Info, f RawTree)
boilerplate f info pt = boilerplateImpl (f $ ptName pt) info pt

boilerplate'
  :: ((Text, Text) -> ParserM (f RawTree))
  -> RawInfo
  -> ParseTree RawTree
  -> ParserM (Product Info, f RawTree)
boilerplate' f info pt@(ParseTree ty _ src) = boilerplateImpl (f (ty, src)) info pt

fallthrough :: ParserM a
fallthrough = lift $ throwIO HandlerFailed

noMatch :: RawInfo -> ParseTree it -> ParserM (Product Info, Error it)
noMatch (r, _) (ParseTree _ children source) = withComments $ pure
  ( [] :> r :> CodeSource source :> Nil
  , Error ("Unrecognized: " <> source) children
  )
