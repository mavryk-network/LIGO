module Test.Diagnostics
  ( unit_bad_parse
  ) where

import AST.Scope (ScopingSystem (FallbackScopes))

import Test.Common.Diagnostics (parseDiagnosticsDriver, simpleTest)

import Test.Tasty.HUnit (Assertion)

-- Try to parse a file, and check that the proper error messages are generated
unit_bad_parse :: HasCallStack => Assertion
unit_bad_parse = parseDiagnosticsDriver FallbackScopes =<< simpleTest
