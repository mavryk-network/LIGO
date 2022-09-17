module AST.Scope.FromCompiler
  ( FromCompiler
  ) where

import Control.Category ((>>>))
import Control.Comonad.Cofree (Cofree (..), _extract)
import Control.Lens (view)
import Data.Either (partitionEithers)
import Data.Foldable (foldrM)
import Data.Function (on)
import Data.Functor ((<&>))
import Data.HashMap.Strict ((!?))
import Data.Map (Map)
import Data.Map qualified as Map
import Data.String (IsString)
import Data.Text (Text)
import Duplo.Lattice
import UnliftIO.Directory (canonicalizePath)

import AST.Scope.Common
import AST.Scope.ScopedDecl (DeclarationSpecifics (..), ScopedDecl (..),
                             Type (ArrowType), TypeDeclSpecifics (..),
                             ValueDeclSpecifics (..), _tdsInit)
import AST.Scope.ScopedDecl.Parser (parseTypeDeclSpecifics)
import AST.Skeleton (Lang, SomeLIGO (..))
import Cli
import Diagnostic (Message (..), MessageDetail (..), Severity (..))
import ListZipper (atLocus, find, withListZipper)
import Log (Log)
import Log qualified
import Range

data FromCompiler

instance (HasLigoClient m, Log m) => HasScopeForest FromCompiler m where
  scopeContract tempSettings (FindContract src (SomeLIGO dialect _) msgs) = do
    defs <- getLigoDefinitions tempSettings src
    (forest, msgs') <- fromCompiler dialect defs
    pure $ FindContract src forest (msgs <> msgs')

-- | Extract `ScopeForest` from LIGO scope dump.
fromCompiler :: forall m. Log m => Lang -> LigoDefinitions -> m (ScopeForest, [Message])
fromCompiler dialect (LigoDefinitions errors warnings decls scopes) = do
  let msgs = maybe [] (map fromLigoErrorToMsg) (errors <> warnings)
  foldrM
    (\scope (sf, errs) -> buildTree decls scope sf <&> fmap (<> errs))
    (emptyScopeForest, msgs)
    scopes
  where
    -- For a new scope to be injected, grab its range and decl and start
    -- injection process.
    buildTree :: LigoDefinitionsInner -> LigoScope -> ScopeForest -> m (ScopeForest, [Message])
    buildTree (LigoDefinitionsInner decls') (LigoScope r es _) sf = do
      r' <- normalizeRange $ fromLigoRangeOrDef r
      (errs, decodedDecls) <- partitionEithers <$> traverse (decodeOrReport r') es
      let ds = Map.fromList decodedDecls
      let rs = Map.keysSet ds
      pure (injectScope ((rs, r') :< []) ds sf, errs)
      where
        decodeOrReport :: Range -> Text -> m (Either Message (DeclRef, ScopedDecl))
        decodeOrReport range decl = case decls' !? decl of
          Nothing -> do
            let
              err :: IsString s => s
              err = [Log.i|Failed to decode #{decl}. This is a bug, please report it.|]
              msg = Message
                { mMessage = FromLanguageServer err
                , mSeverity = SeverityError
                , mRange = range
                }
            Left msg <$ $(Log.err) err
          Just decoded ->
            Right <$> fromLigoDecl decoded

    normalizeRange :: Range -> m Range
    normalizeRange = rFile canonicalizePath

    -- LIGO compiler provides no comments, so they left [].
    fromLigoDecl :: LigoDefinitionScope -> m (DeclRef, ScopedDecl)
    fromLigoDecl (LigoDefinitionScope n orig bodyR ty refs) = do
      r <- normalizeRange . fromLigoRangeOrDef $ orig
      rs <- mapM (normalizeRange . fromLigoRangeOrDef) refs
      let _vdsInitRange = mbFromLigoRange bodyR
          _vdsTspec = parseTypeDeclSpecifics . fromLigoTypeFull <$> ty
          -- `get scope` doesn't provide information about arguments.
          -- `_vdsParams` equals to `Nothing` maeans that it isn't a function.
          -- FIXME (LIGO-679)
          _vdsParams = case _vdsTspec of
                        Just TypeDeclSpecifics{_tdsInit = ArrowType _ _} -> Just []
                        _ -> Nothing
          vspec = ValueDeclSpecifics{ .. }
      pure ( DeclRef n r
           , ScopedDecl n r (r : rs) [] dialect (ValueSpec vspec)
           )

    -- Find a place for a scope inside a ScopeForest.
    injectScope :: ScopeTree -> Map DeclRef ScopedDecl -> ScopeForest -> ScopeForest
    injectScope subject ds' (ScopeForest forest ds) =
        ScopeForest (loop forest) (ds <> ds')
      where
        loop
          = withListZipper
          $ find (subject `isCoveredBy`) >>> atLocus maybeLoop

        isCoveredBy = leq `on` snd . view _extract

        -- If there are no trees above subject here, just put it in.
        -- Otherwise, put it in a tree that covers it.
        maybeLoop :: Maybe ScopeTree -> Maybe ScopeTree
        maybeLoop = Just . maybe subject restart

        -- Take a forest out of tree, loop, put it back.
        restart (r :< trees) = r :< loop trees
