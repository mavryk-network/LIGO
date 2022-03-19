module Test.Integrational.Capabilities.Completion
  ( test_completion
  ) where

import AST.Scope (Standard)

import Test.Common.Capabilities.Completion
import Test.Tasty (TestTree, testGroup)

test_completion :: IO TestTree
test_completion = testGroup "Simple completion" <$> sequenceA
  [ completionDriver @Standard caseInfos
  --, completionDriver @FromCompiler caseInfos  -- FIXME (LIGO-93) (LIGO-208)
  ]
