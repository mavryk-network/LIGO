module Test.Integrational.Capabilities.Hover
  ( unit_hover_apply_type
  , unit_hover_inferred_simple_from_compiler
  , unit_hover_inferred_recursion_from_compiler
  , unit_hover_inferred_simple_fallback
  , unit_hover_inferred_recursion_fallback
  ) where

import Test.HUnit (Assertion)

import AST.Scope (Fallback, Standard)

import qualified Test.Common.Capabilities.Hover as Hover
  (unit_hover_apply_type, unit_hover_inferred_recursion, unit_hover_inferred_simple)
import Test.Common.FixedExpectations (anyException, shouldThrow)

unit_hover_apply_type :: Assertion
unit_hover_apply_type = Hover.unit_hover_apply_type @Standard

unit_hover_inferred_simple_from_compiler :: Assertion
unit_hover_inferred_simple_from_compiler =
  Hover.unit_hover_inferred_simple @Standard

unit_hover_inferred_recursion_from_compiler :: Assertion
unit_hover_inferred_recursion_from_compiler =
  Hover.unit_hover_inferred_recursion @Standard

-- (LIGO-243) Fallback scopes can't infer types and we rely on LIGO for this.
unit_hover_inferred_simple_fallback :: Assertion
unit_hover_inferred_simple_fallback =
  Hover.unit_hover_inferred_simple @Fallback `shouldThrow` anyException

unit_hover_inferred_recursion_fallback :: Assertion
unit_hover_inferred_recursion_fallback =
  Hover.unit_hover_inferred_recursion @Fallback `shouldThrow` anyException
