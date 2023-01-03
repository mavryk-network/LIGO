module Test.Common.Capabilities.Hover
  ( contractsDir
  , HoverKind (..)
  , HoverTest (..)
  , hover'
  , checkHover
  , unit_hover_apply_type
  , unit_hover_inferred_simple
  , unit_hover_inferred_recursion
  , unit_hover_arrow_type
  , unit_hover_arrow_type_mligo
  , unit_hover_arrow_type_jsligo
  , unit_hover_sum_type_jsligo
  , unit_hover_sum_type_mligo
  , unit_hover_sum_type_pascaligo
  , unit_hover_sum_type_religo
  , unit_hover_parametric_type_ligo
  ) where

import Prelude hiding (Type)

import Language.LSP.Types (Hover (..), HoverContents (..), MarkupContent (..))
import System.Directory (makeAbsolute)
import System.FilePath ((</>))

import Test.HUnit (Assertion)

import AST.Capabilities.Hover (hoverDecl)
import AST.Pretty (ppToText)
import AST.Scope.ScopedDecl (Type (..), lppLigoLike)
import AST.Skeleton (Lang (..))

import Range (Range (..), interval, point)

import Test.Common.Capabilities.Util qualified as Common (contractsDir)
import Test.Common.FixedExpectations (expectationFailure, shouldBe)
import Test.Common.Util (ScopeTester, readContractWithScopes)

contractsDir :: FilePath
contractsDir = Common.contractsDir </> "hover"

data HoverKind = Type | Module | Value

data HoverTest = HoverTest
  { htDefinition :: Range
  , htName :: Text
  , htType :: Type
  , htDoc :: [Text]
  , htDialect :: Lang
  , htKind :: HoverKind
  }

hover' :: Range -> Text -> Type -> Lang -> HoverKind -> HoverTest
hover' definition name type' = HoverTest definition name type' []

checkHover :: forall parser. ScopeTester parser => FilePath -> Range -> HoverTest -> Assertion
checkHover fp reference HoverTest{..} = do
  contract <- readContractWithScopes @parser fp
  case hoverDecl reference contract of
    Nothing -> expectationFailure "Expected a hover definition, but got Nothing"
    Just (Hover (HoverContents (MarkupContent _ (lines -> (ty : _ : def : doc)))) _) -> do
      ty `shouldBe` case htKind of
        Type   -> "type " <> htName
        Module -> "module " <> htName
        Value  -> htName <> " : " <> show (lppLigoLike htDialect htType)
      def `shouldBe` ("*defined at* " <> ppToText htDefinition)
      case doc of
        [] -> unless (null htDoc) $ expectationFailure "Expected no documentation, but got some"
        _  -> Just (ppToText htDoc) `shouldBe` find (/= "") doc
    _ -> expectationFailure "Hover definition is not of the expected type"

unit_hover_arrow_type :: forall parser. ScopeTester parser => Assertion
unit_hover_arrow_type = do
  fp <- makeAbsolute $ contractsDir </> "arrow-type.ligo"
  let type' = ArrowType (ApplyType (AliasType "list") [AliasType "int"]) (AliasType "int")
  checkHover @parser fp (interval 7 3 10){_rFile = fp} (hover' (interval 1 10 17){_rFile = fp} "fold_op" type' Pascal Value)

unit_hover_arrow_type_mligo :: forall parser. ScopeTester parser => Assertion
unit_hover_arrow_type_mligo = do
  fp <- makeAbsolute $ contractsDir </> "arrow-type.mligo"
  let type' = ArrowType (ApplyType (AliasType "list") [AliasType "int"]) (AliasType "unit")
  checkHover @parser fp (interval 5 34 41){_rFile = fp} (hover' (interval 1 5 12){_rFile = fp} "iter_op" type' Caml Value)

unit_hover_arrow_type_jsligo :: forall parser. ScopeTester parser => Assertion
unit_hover_arrow_type_jsligo = do
  fp <- makeAbsolute $ contractsDir </> "arrow-type.jsligo"
  let type' = ArrowType (ApplyType (AliasType "list") [AliasType "int"]) (AliasType "unit")
  checkHover @parser fp (interval 6 12 19){_rFile = fp} (hover' (interval 1 5 12){_rFile = fp} "iter_op" type' Js Value)

unit_hover_apply_type :: forall parser. ScopeTester parser => Assertion
unit_hover_apply_type = do
  fp <- makeAbsolute $ contractsDir </> "apply-type.ligo"
  let type' = ApplyType (AliasType "contract") [AliasType "unit"]
  checkHover @parser fp (point 3 23){_rFile = fp} (hover' (interval 2 9 10){_rFile = fp} "c" type' Pascal Value)

unit_hover_inferred_simple :: forall parser. ScopeTester parser => Assertion
unit_hover_inferred_simple = do
  fp <- makeAbsolute $ contractsDir </> "simple.mligo"
  let type' = AliasType "int"
  checkHover @parser fp (point 2 9){_rFile = fp} (hover' (interval 1 5 6){_rFile = fp} "x" type' Caml Value)
  checkHover @parser fp (point 2 5){_rFile = fp} (hover' (interval 2 5 6){_rFile = fp} "y" type' Caml Value)

unit_hover_inferred_recursion :: forall parser. ScopeTester parser => Assertion
unit_hover_inferred_recursion = do
  fp <- makeAbsolute $ contractsDir </> "recursion.mligo"
  let type' = AliasType "int"
  checkHover @parser fp (point 2 21){_rFile = fp} (hover' (interval 1 18 21){_rFile = fp} "acc" type' Caml Value)

unit_hover_sum_type_jsligo :: forall parser. ScopeTester parser => Assertion
unit_hover_sum_type_jsligo = do
  fp <- makeAbsolute $ contractsDir </> "sum.jsligo"
  let type' = AliasType "parameter"
  checkHover @parser fp (point 13 12){_rFile = fp} (hover' (interval 10 16 22){_rFile = fp} "action" type' Js Value)

unit_hover_sum_type_mligo :: forall parser. ScopeTester parser => Assertion
unit_hover_sum_type_mligo = do
  fp <- makeAbsolute $ contractsDir </> "sum.mligo"
  let type' = AliasType "parameter"
  checkHover @parser fp (point 13 12){_rFile = fp} (hover' (interval 10 11 17){_rFile = fp} "action" type' Caml Value)

unit_hover_sum_type_pascaligo :: forall parser. ScopeTester parser => Assertion
unit_hover_sum_type_pascaligo = do
  fp <- makeAbsolute $ contractsDir </> "sum.ligo"
  let type' = AliasType "parameter"
  checkHover @parser fp (point 13 10){_rFile = fp} (hover' (interval 10 22 28){_rFile = fp} "action" type' Pascal Value)

unit_hover_sum_type_religo :: forall parser. ScopeTester parser => Assertion
unit_hover_sum_type_religo = do
  fp <- makeAbsolute $ contractsDir </> "sum.religo"
  let type' = AliasType "parameter"
  checkHover @parser fp (point 13 13){_rFile = fp} (hover' (interval 10 14 20){_rFile = fp} "action" type' Reason Value)

unit_hover_parametric_type_ligo :: forall parser. ScopeTester parser => Assertion
unit_hover_parametric_type_ligo = do
  fp <- makeAbsolute $ contractsDir </> "parametric.ligo"
  let type' = AliasType "a"
  checkHover @parser fp (point 1 56){_rFile = fp} (hover' (interval 1 12 13){_rFile = fp} "a" type' Pascal Type)
