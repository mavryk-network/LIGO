module Test.Integrational.Cli
  ( test_ligo_159
  ) where

import Data.Foldable (asum)
import System.FilePath ((</>))
import UnliftIO.Exception (SomeException, fromException, tryJust)

import Cli
import Log (runNoLoggingT)
import ParseTree (pathToSrc)

import Test.Common.FixedExpectations (HasCallStack, expectationFailure)
import Test.Tasty (TestTree, testGroup)
import Test.Tasty.HUnit (testCase)

filterException :: SomeException -> Maybe SomeLigoException
filterException e = asum
  [ SomeLigoException <$> fromException @LigoDecodedExpectedClientFailureException e
  , SomeLigoException <$> fromException @LigoErrorNodeParseErrorException          e
  , SomeLigoException <$> fromException @LigoUnexpectedCrashException              e
  ]

checkFile :: HasCallStack => FilePath -> TestTree
checkFile path = testCase path do
  src <- pathToSrc path
  tryJust filterException (runNoLoggingT $ getLigoDefinitions src) >>= \case
    Left  _ -> pure ()
    Right _ -> expectationFailure "Expected contract to fail, but it has succeeded."

test_ligo_159 :: TestTree
test_ligo_159 = testGroup "Contracts should throw errors" $ checkFile <$> files
  where
    files :: [FilePath]
    files = (\f -> "test" </> "contracts" </> "json-bugs" </> f) <$>
      [ -- TODO: "LIGO-159_2.mligo"
        "LIGO-159_3.mligo"
      ]
