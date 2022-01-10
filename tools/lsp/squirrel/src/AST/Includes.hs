{-# LANGUAGE OverloadedLists #-}

module AST.Includes
  ( extractIncludedFiles
  , includesGraph
  , insertPreprocessorRanges
  , getMarkers
  , getMarkerInfos
  , MarkerInfo (..)
  ) where

import Algebra.Graph.AdjacencyMap (AdjacencyMap)
import Algebra.Graph.AdjacencyMap qualified as G
import Control.Arrow (first)
import Control.Lens (Lens', _1, to, view, (&), (+~), (-~), (.~), (^.))
import Control.Monad (forM, join, when)
import Control.Monad.IO.Class (MonadIO)
import Control.Monad.RWS.Strict (RWS, RWST, execRWS, execRWST, gets, modify, tell)
import Data.Bifunctor (bimap)
import Data.Bool (bool)
import Data.DList (DList, toList)
import Data.Foldable (for_)
import Data.Functor ((<&>))
import Data.IntMap.Strict (IntMap)
import Data.IntMap.Strict qualified as IntMap
import Data.List (sortOn)
import Data.Map.Strict (Map)
import Data.Map.Strict qualified as Map
import Data.Text qualified as Text (pack)
import Data.Word (Word32)
import Duplo.Tree (Cofree ((:<)), inject)
import System.FilePath ((</>), takeDirectory)
import UnliftIO.Directory (canonicalizePath)

import AST.Scope.Common (ContractInfo, pattern FindContract, ParsedContractInfo, contractFile, MarkerInfo (..))
import AST.Scope.Fallback (loopM, loopM_)
import AST.Skeleton (Error (..), Lang (..), LIGO, SomeLIGO (..))

import Parser
  ( CodeSource (..), Info, LineMarker (..), LineMarkerType (..), ParsedInfo
  , ShowRange (N)
  )
import ParseTree (Source (..))
import Product (Contains, Product (..), getElem, modElem, putElem)
import Range
  ( PreprocessedRange (..), Range, getRange, point, rangeLines, rFile, rFinish
  , rStart, startLine, finishLine
  )

fromOriginalPoint :: Product Info -> Product ParsedInfo
fromOriginalPoint infos = PreprocessedRange (getRange infos) :> infos

insertPreprocessorRanges :: MonadIO m => ContractInfo -> m ParsedContractInfo
insertPreprocessorRanges = fmap fst . extractIncludedFiles False

getMarkers :: forall xs. Contains [LineMarker] xs => LIGO xs -> [LineMarker]
getMarkers ligo = toList $ snd $ execRWS (loopM_ collectMarkers ligo) () ()
  where
    collectMarkers :: LIGO xs -> RWS () (DList LineMarker) () ()
    collectMarkers (info :< _) = for_ (getElem @[LineMarker] info) (tell . pure)

getMarkerInfos
  :: MonadIO m
  => Bool
  -> FilePath
  -> [LineMarker]
  -> m (IntMap MarkerInfo, DList (FilePath, FilePath))
getMarkerInfos directIncludes pwd markers =
  execRWST (collectMarkerInfos directIncludes pwd markers) () mempty

collectMarkerInfos
  :: MonadIO m
  => Bool
  -> FilePath
  -> [LineMarker]
  -> RWST () (DList (FilePath, FilePath)) (IntMap MarkerInfo) m ()
collectMarkerInfos directIncludes pwd markers =
  -- For the actual ranges, we pretend everything is fused into the line
  -- where the include would be located.
  -- For the preprocessed ranges, we use the line markers to map the range
  -- to the file and line they represent.
  for_ (sortOn (view startLine . lmLoc) markers) \lm@(LineMarker next f _ r) -> do
    let line = r ^. startLine . to fromIntegral
    n <- withPwd pwd next
    case f of
      RootFile     -> modify $ IntMap.insert line $ MarkerInfo lm r 0
      IncludedFile -> gets (IntMap.lookupLT line) >>= \case
        Nothing -> pure ()
        Just (_, MarkerInfo prev lr d) -> do
          modify $ IntMap.insert
            line
            (MarkerInfo lm (bool lr (r & startLine -~ lmLoc prev ^. finishLine - lmLine prev) $ d == 0) (d + 1))
          when (directIncludes `implies` d == 0) $ do
            p <- withPwd pwd (lmFile prev)
            tell [(p, n)]
      ReturnToFile -> modify $ IntMap.lookupLT line >>= \case
        Nothing -> id
        Just (_, MarkerInfo _ lr d) -> IntMap.insert line $ MarkerInfo lm lr (d - 1)

  where
    implies :: Bool -> Bool -> Bool
    prerequisite `implies` conclusion = not prerequisite || conclusion
    infixr 1 `implies`

withPwd :: MonadIO m => FilePath -> FilePath -> m FilePath
withPwd pwd = canonicalizePath . (pwd </>)

-- | Given some contract A, returns a new version of that contract such that:
-- * When a line marker is found, all 'Range's will be adjusted in a manner such
--   that everything in the current document will stay as it is, but ranges from
--   included parts will be "squashed" to the 'Range' where the inclusion was
--   made. In other words, the ranges will be restored to the 'Range's that the
--   user sees.
-- * Additionally, 'PreprocessedRange's will be included, and they will have the
--   actual 'Range's of the files. That is, if A includes B then the ranges will
--   actually point to B. These are the actual ranges which are mapped according
--   to the line markers present in the document.
--
-- Also returns a difference list containing the edges of how to include files.
-- That is, if A includes B, then this list will contain a tuple (A, B).
extractIncludedFiles
  :: forall m. MonadIO m
  => Bool  -- ^ Whether to only extract directly included files ('True') or all of them ('False').
  -> ContractInfo  -- ^ The contract to scan for includes.
  -> m (ParsedContractInfo, DList (FilePath, FilePath))
extractIncludedFiles directIncludes (FindContract file (SomeLIGO dialect ligo) msgs) = do
  let markers = getMarkers ligo
  (markerInfos, edges) <- getMarkerInfos directIncludes pwd markers
  (info :< tree) <- loopM (go markerInfos) $ fromOriginalPoint <$> ligo
  let
    -- We still want RawContract to start at line 1:
    ligo' = bool (modElem (startLine +~ 1) info :< tree) (info :< tree) (IntMap.null markerInfos)
    msgs' = fmap (first (adjustRange markerInfos)) msgs
  pure (FindContract file (SomeLIGO dialect ligo') msgs', edges)
  where
    pwd :: FilePath
    pwd = takeDirectory $ srcPath file

    go :: IntMap MarkerInfo -> LIGO ParsedInfo -> m (LIGO ParsedInfo)
    go markers (info :< tree) = do
      info' <- info
            &  adjustPreprocessedRange markers
           <&> modElem (adjustRange markers)
      pure (info' :< tree)

    adjustPreprocessedRange :: IntMap MarkerInfo -> Product ParsedInfo -> m (Product ParsedInfo)
    adjustPreprocessedRange markers i = case prev of
      Nothing -> pure i
      Just (_, MarkerInfo marker _ _) -> do
        normalized <- withPwd pwd (lmFile marker)
        let preRange = range
              & rangeLines -~ (lmLoc marker ^. finishLine - lmLine marker)
              & rFile .~ normalized
        pure (putElem (PreprocessedRange preRange) i)
      where
        prev = IntMap.lookupLE (range ^. startLine . to fromIntegral) markers
        range = getRange i

    adjustSide :: Lens' Range (Word32, Word32, Word32)
               -> IntMap MarkerInfo
               -> Range
               -> Range
    adjustSide side markers range = case prev of
      Nothing -> range
      Just (_, MarkerInfo marker lastRange depth) ->
        -- In case we are at depth 0, we need to subtract the accumulated range at
        -- the both the start and finish, since we may see new includes while
        -- traversing the tree.
        let newRange = range &
              if depth > 0 then
                side .~ lastRange ^. rStart
              else
                (side . _1) -~ lmLoc marker ^. finishLine - lmLine marker
         in newRange
      where
        prev = IntMap.lookupLE (range ^. (side . _1 . to fromIntegral)) markers

    adjustRange :: IntMap MarkerInfo -> Range -> Range
    adjustRange markers = adjustSide rFinish markers
                        . adjustSide rStart markers

-- | Given a list of contracts, builds a graph that represents how they are
-- included.
includesGraph :: forall m. MonadIO m => [ContractInfo] -> m (AdjacencyMap ParsedContractInfo)
includesGraph contracts = do
  knownContracts :: Map FilePath (ParsedContractInfo, DList (FilePath, FilePath))
    <- fmap Map.fromList $ forM contracts $ \c -> do
         included <- extractIncludedFiles False c
         pure (contractFile c, included)

  let findContract :: FilePath -> (ParsedContractInfo, DList (FilePath, FilePath))
      findContract contract =
        Map.findWithDefault (emptyContract contract, []) contract knownContracts

      go
        :: ContractInfo
        -> (DList (ParsedContractInfo, ParsedContractInfo), [ParsedContractInfo])
        -> (DList (ParsedContractInfo, ParsedContractInfo), [ParsedContractInfo])
      go contract (edges, vertices) =
        let
          (vertex', edges') = findContract $ contractFile contract
          edges'' = join bimap (fst . findContract) <$> edges'
        in
        (edges'' <> edges, vertex' : vertices)

  pure (uncurry G.overlay $ bimap (G.edges . toList) G.vertices $ foldr go ([], []) contracts)

  where
    emptyContract :: FilePath -> ParsedContractInfo
    emptyContract name =
      let
        p = point 0 0
        info = PreprocessedRange p :> [] :> [] :> p :> N :> CodeSource "" :> Nil
      in
      FindContract
        (Path name)
        (SomeLIGO Caml (info :< inject (Error ("Missing contract: " <> Text.pack name) [])))
        []
