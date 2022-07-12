module Test.Diagnostics
  ( unit_bad_parse
  , unit_name_not_found
  ) where

import Test.Common.Diagnostics
  ( DiagnosticSource (FallbackSource), parseDiagnosticsDriver, simpleTest
  , treeDoesNotContainNameTest
  )
import Test.Common.FixedExpectations (HasCallStack)
import Test.Tasty.HUnit (Assertion)

-- Try to parse a file, and check that the proper error messages are generated
unit_bad_parse :: HasCallStack => Assertion
unit_bad_parse = parseDiagnosticsDriver FallbackSource =<< simpleTest

unit_name_not_found :: HasCallStack => Assertion
unit_name_not_found = parseDiagnosticsDriver FallbackSource =<< treeDoesNotContainNameTest
