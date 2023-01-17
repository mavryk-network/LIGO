module Test.Integrational.Capabilities.Hover
  ( unit_hover_apply_type
  , unit_hover_inferred_simple_from_compiler
  , unit_hover_inferred_recursion_from_compiler
  , unit_hover_inferred_simple_fallback
  , unit_hover_inferred_recursion_fallback
  , unit_hover_arrow_type
  , unit_hover_arrow_type_jsligo
  , unit_hover_arrow_type_mligo
  , unit_hover_sum_type_jsligo
  , unit_hover_sum_type_mligo
  , unit_hover_sum_type_pascaligo
  , unit_hover_parametric_type_ligo
  , unit_hover_parameter_type_jsligo
  , unit_hover_parameter_type_mligo
  , unit_hover_parameter_type_pligo
  , unit_hover_module_type_access_mligo
  ) where

import Test.HUnit (Assertion)

import AST.Scope (Fallback, FromCompiler, Standard)

import Test.Common.Capabilities.Hover qualified as Hover
import Test.Common.FixedExpectations (anyException, shouldThrow)

unit_hover_arrow_type :: Assertion
unit_hover_arrow_type = do
  Hover.unit_hover_arrow_type @Standard
  Hover.unit_hover_arrow_type @FromCompiler

unit_hover_arrow_type_mligo :: Assertion
unit_hover_arrow_type_mligo = do
  Hover.unit_hover_arrow_type_mligo @Standard
  Hover.unit_hover_arrow_type_mligo @FromCompiler

unit_hover_arrow_type_jsligo :: Assertion
unit_hover_arrow_type_jsligo = do
  Hover.unit_hover_arrow_type_jsligo @Standard
  Hover.unit_hover_arrow_type_jsligo @FromCompiler

unit_hover_apply_type :: Assertion
unit_hover_apply_type = do
  Hover.unit_hover_apply_type @Standard
  Hover.unit_hover_apply_type @FromCompiler

unit_hover_inferred_simple_from_compiler :: Assertion
unit_hover_inferred_simple_from_compiler = do
  Hover.unit_hover_inferred_simple @Standard
  Hover.unit_hover_inferred_simple @FromCompiler

unit_hover_inferred_recursion_from_compiler :: Assertion
unit_hover_inferred_recursion_from_compiler = do
  Hover.unit_hover_inferred_recursion @Standard
  Hover.unit_hover_inferred_recursion @FromCompiler

unit_hover_inferred_recursion_fallback :: Assertion
unit_hover_inferred_recursion_fallback =
  Hover.unit_hover_inferred_recursion @Fallback

-- (LIGO-243) Fallback scopes can't infer types and we rely on LIGO for this.
unit_hover_inferred_simple_fallback :: Assertion
unit_hover_inferred_simple_fallback =
  Hover.unit_hover_inferred_simple @Fallback `shouldThrow` anyException

unit_hover_sum_type_jsligo :: Assertion
unit_hover_sum_type_jsligo = do
  Hover.unit_hover_sum_type_jsligo @Standard
  Hover.unit_hover_sum_type_jsligo @FromCompiler

unit_hover_sum_type_mligo :: Assertion
unit_hover_sum_type_mligo = do
  Hover.unit_hover_sum_type_mligo @Standard
  Hover.unit_hover_sum_type_mligo @FromCompiler

unit_hover_sum_type_pascaligo :: Assertion
unit_hover_sum_type_pascaligo = do
  Hover.unit_hover_sum_type_pascaligo @Standard
  Hover.unit_hover_sum_type_pascaligo @FromCompiler

unit_hover_parametric_type_ligo :: Assertion
unit_hover_parametric_type_ligo = do
  Hover.unit_hover_parametric_type_ligo @Standard
  -- TODO: We don't decode the origin of a type variable and LIGO doesn't provide enough information
  -- about them.
  --Hover.unit_hover_parametric_type_ligo @FromCompiler

unit_hover_parameter_type_jsligo :: Assertion
unit_hover_parameter_type_jsligo = do
  Hover.unit_hover_parameter_type_jsligo @Standard
  Hover.unit_hover_parameter_type_jsligo @FromCompiler

unit_hover_parameter_type_mligo :: Assertion
unit_hover_parameter_type_mligo = do
  Hover.unit_hover_parameter_type_mligo @Standard
  Hover.unit_hover_parameter_type_mligo @FromCompiler

unit_hover_parameter_type_pligo :: Assertion
unit_hover_parameter_type_pligo = do
  Hover.unit_hover_parameter_type_pligo @Standard
  Hover.unit_hover_parameter_type_pligo @FromCompiler

unit_hover_module_type_access_mligo :: Assertion
unit_hover_module_type_access_mligo = do
  Hover.unit_hover_module_type_access_mligo @Standard
  Hover.unit_hover_module_type_access_mligo @FromCompiler
