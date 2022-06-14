module Language.LIGO.Debugger.Michelson
  ( DecodeError (..)
  , EmbedError
  , readLigoMapper
  ) where

import Unsafe qualified

import Control.Monad.Except (Except, runExcept, throwError)
import Data.Char (isAsciiUpper, isDigit)
import Data.Coerce (coerce)
import Data.Default (def)
import Data.Set qualified as Set
import Data.Text qualified as Text
import Data.Vector qualified as V
import Fmt (Buildable (..), Builder, genericF)
import Morley.Debugger.Core.Common (debuggerTcOptions)
import Morley.Debugger.Core.Navigate (SourceLocation (..), SourceType (..))
import Morley.Micheline.Class (FromExpressionError, fromExpression)
import Morley.Micheline.Expression
  (Expression (..), MichelinePrimAp (..), MichelinePrimitive (..), michelsonPrimitive)
import Morley.Michelson.ErrorPos (Pos (..), SrcPos (..), mkPos)
import Morley.Michelson.TypeCheck (TCError, typeCheckContract, typeCheckingWith)
import Morley.Michelson.Typed
  (Contract' (..), CtorEffectsApp (..), DfsSettings (..), Instr (..), SomeContract (..),
  SomeMeta (SomeMeta), dfsTraverseInstr, isMichelsonInstr)
import Text.Interpolation.Nyan

import Language.LIGO.Debugger.CLI.Types
import Language.LIGO.Debugger.Types

-- | When it comes to information attached to entries in Michelson code,
-- so-called table encoding stands for representing that info in a list
-- in the order of DFS traversal over Micheline tree.
--
-- This type stands for index in such list, i.e. it is number of the
-- Micheline node that we will visit if we go with DFS.
newtype TableEncodingIdx = TableEncodingIdx { unTableEncodingIdx :: Int }
  deriving stock (Show, Eq, Ord)
  deriving newtype (Buildable)

-- | Enumerates all the Micheline nodes starting from 0 and
-- returns only those indices that correspond to actual instructions.
extractInstructionsIndexes :: Expression -> [TableEncodingIdx]
extractInstructionsIndexes =
  -- We drop the head since we are not interested in the initial seq.
  -- Why dropping the second element - is yet a mistery
  drop 2 . evaluatingState 0 . go
  where
    go :: Expression -> State Int [TableEncodingIdx]
    go = \case
      ExpressionInt _ -> skip
      ExpressionString _ -> skip
      ExpressionBytes _ -> skip
      ExpressionSeq exprs -> addFold exprs
      ExpressionPrim MichelinePrimAp {mpaPrim, mpaArgs}
        | Set.member (coerce mpaPrim) primInstrs -> addFold mpaArgs
        | otherwise -> modify (+ 1) *> (fold <$> traverse go mpaArgs)

    skip :: State Int [TableEncodingIdx]
    skip = mempty <$ modify (+ 1)

    addFold :: [Expression] -> State Int [TableEncodingIdx]
    addFold exprs = do
      index <- get
      put $ index + 1
      (TableEncodingIdx index :) . fold <$> traverse go exprs

    prims, primInstrs :: Set Text
    primInstrs = Set.filter (Text.all (\c -> isAsciiUpper c || isDigit c || c == '_')) prims
    prims = Set.fromList $ toList michelsonPrimitive

data DecodeError
  = FromExpressionFailed FromExpressionError
  | TypeCheckFailed TCError
  | InsufficientMeta TableEncodingIdx
  | MetaEmbeddingError EmbedError
  deriving stock (Eq, Generic)

instance Buildable DecodeError where
  build = genericF

fromExpressionToTyped
  :: Expression
  -> Either DecodeError SomeContract
fromExpressionToTyped expr = do
  uContract <- first FromExpressionFailed $ fromExpression expr
  first TypeCheckFailed $ typeCheckingWith debuggerTcOptions $ typeCheckContract uContract

-- Using proper content in this type is too inconvenient at the moment
data EmbedError
  = RemainingExtraEntries Builder
  | InsufficientEntries Builder
  deriving stock (Show, Eq)

instance Buildable EmbedError where
  build = \case
    RemainingExtraEntries msg -> build msg
    InsufficientEntries msg -> build msg

-- | Embed data into typed instructions visiting them in DFS order.
embedInInstr
  :: forall meta inp out.
     (Show meta, Typeable meta, NFData meta)
  => [meta]
  -> Instr inp out
  -> Either EmbedError (Instr inp out)
embedInInstr metaTape instr = do
  (resInstr, tapeRest) <- runExcept $ usingStateT metaTape $
    dfsTraverseInstr def{ dsGoToValues = True, dsCtorEffectsApp = recursionImpl } pure instr
  unless (null tapeRest) $
    Left . RemainingExtraEntries $
      [int||Too many left entries, remaining are: #s{tapeRest}|]
  return resInstr
  where
    isActualInstr = \case
      Seq{} -> False
      i -> isMichelsonInstr i

    recursionImpl :: CtorEffectsApp $ StateT [meta] $ Except EmbedError
    recursionImpl = CtorEffectsApp "embed" $ \oldInstr mkNewInstr ->
      if not $ isActualInstr oldInstr
      then mkNewInstr
      else get >>= \case
        [] -> throwError . InsufficientEntries $
          [int||Insufficient number of entries, broke at #{oldInstr}|]
        (meta : rest) -> do
          put rest
          Meta (SomeMeta meta) <$> mkNewInstr

-- | Read LIGO's debug output and produce
--
-- 1. All locations that may be worth attention. This is to be used
--    in switching breakpoints.
-- 2. A contract with inserted @Meta (SomeMeta (info :: 'EmbeddedLigoMeta'))@
--    wrappers that carry the debug info.
readLigoMapper
  :: LigoMapper
  -> Either DecodeError (Set SourceLocation, SomeContract)
readLigoMapper ligoMapper = do
  let indexes :: [TableEncodingIdx] =
        extractInstructionsIndexes (lmMichelsonCode ligoMapper)
  metaPerInstr :: [LigoIndexedInfo] <-
    forM indexes \i ->
      maybe (Left $ InsufficientMeta i) pure $
        lmLocations ligoMapper V.!? unTableEncodingIdx i

  SomeContract contract <- fromExpressionToTyped (lmMichelsonCode ligoMapper)
  extendedContract <- first MetaEmbeddingError $
    (\code -> SomeContract contract{ cCode = code }) <$>
      embedInInstr @EmbeddedLigoMeta
        metaPerInstr
        (cCode contract)

  let allLocs =
        -- We expect a lot of duplicates, stripping them via putting to Set
        Set.fromList $
        mapMaybe ligoInfoToSourceLoc $ toList $ lmLocations ligoMapper

  -- The LIGO's debug info may be really large, so we better force
  -- the evaluation for all the info that will be stored for the entire
  -- debug session, and let GC wipe out everything intermediate.
  return $! force (allLocs, extendedContract)

  where
    ligoInfoToSourceLoc :: LigoIndexedInfo -> Maybe SourceLocation
    ligoInfoToSourceLoc LigoIndexedInfo{..} = asum
      [ do
          LigoRange{..} <- liiLocation
          return $ SourceLocation (SourcePath lrFile) (convertPos lrStart)

      , do
          -- TODO: liiEnvironment should also give us source location -
          -- point to the beginning of the statement
          _ <- liiEnvironment
          return $ SourceLocation (SourcePath "??") (SrcPos (Pos 0) (Pos 0))
      ]

    convertPos :: LigoPosition -> SrcPos
    convertPos (LigoPosition l c) =
      SrcPos
        (unsafe $ mkPos $ Unsafe.fromIntegral (l - 1))
        (unsafe $ mkPos $ Unsafe.fromIntegral c)
