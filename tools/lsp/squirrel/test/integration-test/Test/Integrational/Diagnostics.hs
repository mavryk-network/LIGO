module Test.Integrational.Diagnostics
  ( unit_bad_parse
  ) where

import AST.Scope (ScopingSystem (CompilerScopes, StandardScopes))

import Test.Common.Diagnostics (parseDiagnosticsDriver, simpleTest)

import Test.Tasty.HUnit (Assertion)

-- Try to parse a file, and check that the proper error messages are generated
unit_bad_parse :: HasCallStack => Assertion
unit_bad_parse = do
  simpleTest' <- simpleTest
  parseDiagnosticsDriver StandardScopes simpleTest'
  parseDiagnosticsDriver CompilerScopes simpleTest'
