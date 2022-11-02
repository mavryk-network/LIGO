{-# LANGUAGE UndecidableInstances #-}

module Test.Util
  ( -- * Shared helpers
    (</>)
  , contractsDir
  , hasLigoExtension
  , pattern SomeLorentzValue

    -- * Test utilities
  , ShowThroughBuild (..)
  , TestBuildable (..)
  , rmode'tb
  , (@?=)
  , (@@?=)
  , (@?)
  , (@@?)
  , (@?==)
  , (@~=?)
  , HUnit.testCase
  , HUnit.testCaseSteps
  , HUnit.assertFailure
  , HUnit.assertBool
    -- * Common snippets
  , intType
    -- * Helpers for breakpoints
  , goToNextBreakpoint
  , goToPreviousBreakpoint
    -- * Snapshot unilities
  , ContractRunData (..)
  , mkSnapshotsFor
  , mkSnapshotsForLogging
  , withSnapshots
  , testWithSnapshotsImpl
  , testWithSnapshots
  , testWithSnapshotsLogging
  , checkSnapshot
  , unexpectedSnapshot
  ) where

import Data.Singletons.Decide (decideEquality)
import Fmt (Buildable (..), blockListF', pretty)
import System.FilePath (takeExtension, (</>))
import Test.HUnit (Assertion)
import Test.Tasty.HUnit qualified as HUnit
import Text.Interpolation.Nyan
import Text.Interpolation.Nyan.Core (RMode (..))
import Text.Show qualified

import Morley.Debugger.Core.Breakpoint
  (BreakpointSelector (NextBreak), continueUntilBreakpoint, reverseContinue)
import Morley.Debugger.Core.Navigate
  (DebugSource (DebugSource), DebuggerState (..), Direction (Backward, Forward),
  FrozenPredicate (FrozenPredicate), NavigableSnapshot (getExecutedPosition), SourceLocation,
  curSnapshot, frozen, groupSourceLocations, isAtBreakpoint, moveTill, playInterpretHistory)
import Morley.Michelson.Runtime.Dummy (dummyContractEnv)
import Morley.Michelson.Typed (SingI (sing))
import Morley.Michelson.Typed qualified as T
import Morley.Util.Typeable

import Language.LIGO.Debugger.CLI.Call
import Language.LIGO.Debugger.CLI.Types
import Language.LIGO.Debugger.Common
import Language.LIGO.Debugger.Handlers.Helpers
import Language.LIGO.Debugger.Michelson
import Language.LIGO.Debugger.Snapshots

contractsDir :: FilePath
contractsDir = "test" </> "contracts"

hasLigoExtension :: FilePath -> Bool
hasLigoExtension file =
  takeExtension file `elem`
    [ ".ligo"
    , ".pligo"
    , ".mligo"
    , ".religo"
    , ".jsligo"
    ]

newtype ShowThroughBuild a = STB
  { unSTB :: a
  } deriving newtype (Eq, Ord)

instance Buildable (TestBuildable a) => Show (ShowThroughBuild a) where
  show = pretty . TB . unSTB

newtype TestBuildable a = TB
  { unTB :: a
  } deriving newtype (Eq, Ord)

-- | Provide @tb@ rendering mode for nyan-interpolators.
rmode'tb :: Buildable (TestBuildable a) => RMode a
rmode'tb = RMode (build . TB)

instance {-# OVERLAPPABLE #-} Buildable a => Buildable (TestBuildable a) where
  build = build . unTB

instance Buildable (TestBuildable a) => Buildable (TestBuildable [a]) where
  build (TB l) = pretty $ blockListF' "-" (build . TB) l

instance (Buildable (TestBuildable e), Buildable (TestBuildable a)) =>
         Buildable (TestBuildable (Either e a)) where
  build (TB res) = case res of
    Right a -> build (TB a)
    Left e -> "Failure: " <> build (TB e)

instance (Buildable (TestBuildable a), Buildable (TestBuildable b)) =>
         Buildable (TestBuildable (a, b)) where
  build (TB (a, b)) =
    [int||
      ( #{TB a}
      , #{TB b}
      )
     |]

(@?=)
  :: (Eq a, Buildable (TestBuildable a), MonadIO m, HasCallStack)
  => a -> a -> m ()
(@?=) a b = liftIO $ STB a HUnit.@?= STB b
infix 1 @?=

-- | Similar to '@?=' but checks monadic value.
(@@?=)
  :: (Eq a, Buildable (TestBuildable a), MonadIO m, HasCallStack)
  => m a -> a -> m ()
(@@?=) am b = am >>= \a -> a @?= b
infix 1 @@?=

-- | Check that value matches certain predicate.
(@?)
  :: (Buildable (TestBuildable a), MonadIO m, HasCallStack)
  => a -> (a -> Bool) -> m ()
(@?) a p
  | p a = pass
  | otherwise = liftIO $ HUnit.assertFailure [int||Unexpected value: #tb{a}|]
infix 1 @?

-- | Similar to '@?' but checks monadic value.
(@@?)
  :: (Buildable (TestBuildable a), MonadIO m, HasCallStack)
  => m a -> (a -> Bool) -> m ()
(@@?) am p = am >>= \a -> a @? p
infix 1 @@?

(@?==)
  :: (MonadIO m, MonadReader r m, Eq a, Buildable (TestBuildable a), HasCallStack)
  => Lens' r a -> a -> m ()
len @?== expected = do
  actual <- view len
  actual @?= expected
infixl 0 @?==

-- | Check that the first list is permutation of the second one.
(@~=?)
  :: (Ord a, Buildable (TestBuildable a), MonadIO m, HasCallStack)
  => [a] -> [a] -> m ()
(@~=?) xs ys = liftIO $
  HUnit.assertBool
    [int||Expected #tb{xs} to be a permutation of #tb{ys}|]
    (xs `isPermutationOf` ys)

intType :: LigoType
intType = LTConstant $
  LigoTypeConstant
    { ltcParameters = []
    , ltcInjection = "Int" :| []
    }

compareWithCurLocation
  :: (MonadState (DebuggerState InterpretSnapshot) m)
  => SourceLocation -> FrozenPredicate (DebuggerState InterpretSnapshot) m
compareWithCurLocation oldSrcLoc = FrozenPredicate $
  getExecutedPosition >>= maybe (pure False) (pure . (/= oldSrcLoc))

goToNextBreakpoint :: (MonadState (DebuggerState InterpretSnapshot) m) => m ()
goToNextBreakpoint = do
  oldSrcLocMb <- frozen getExecutedPosition
  void $ case oldSrcLocMb of
    Just oldSrcLoc -> moveTill Forward (isAtBreakpoint && compareWithCurLocation oldSrcLoc)
    Nothing -> continueUntilBreakpoint NextBreak

goToPreviousBreakpoint :: (MonadState (DebuggerState InterpretSnapshot) m) => m ()
goToPreviousBreakpoint = do
  oldSrcLocMb <- frozen getExecutedPosition
  void $ case oldSrcLocMb of
    Just oldSrcLoc -> moveTill Backward (isAtBreakpoint && compareWithCurLocation oldSrcLoc)
    Nothing -> reverseContinue NextBreak

data ContractRunData =
  forall param st.
  ( T.IsoValue param, T.IsoValue st
  , SingI (T.ToT param), SingI (T.ToT st)
  , T.ForbidOr (T.ToT param)
  )
  => ContractRunData
  { crdProgram :: FilePath
  , crdEntrypoint :: Maybe String
  , crdParam :: param
  , crdStorage :: st
  }

-- | Doesn't log anything.
dummyLoggingFunction :: (Monad m) => String -> m ()
dummyLoggingFunction = const $ pure ()

mkSnapshotsForImpl
  :: HasCallStack
  => (String -> IO ()) -> ContractRunData -> IO (Set SourceLocation, InterpretHistory InterpretSnapshot)
mkSnapshotsForImpl logger (ContractRunData file mEntrypoint (param :: param) (st :: st)) = do
  let entrypoint = mEntrypoint ?: "main"
  ligoMapper <- compileLigoContractDebug entrypoint file
  (exprLocs, T.SomeContract (contract@T.Contract{} :: T.Contract cp' st'), allFiles) <-
    case readLigoMapper ligoMapper typesReplaceRules instrReplaceRules of
      Right v -> pure v
      Left err -> HUnit.assertFailure $ pretty err
  Refl <- sing @cp' `decideEquality` sing @(T.ToT param)
    & maybe (HUnit.assertFailure "Parameter type mismatch") pure
  Refl <- sing @st' `decideEquality` sing @(T.ToT st)
    & maybe (HUnit.assertFailure "Storage type mismatch") pure

  parsedContracts <- parseContracts allFiles

  let statementLocs = getStatementLocs exprLocs parsedContracts

  his <-
    collectInterpretSnapshots
      file
      (fromString entrypoint)
      contract
      T.epcPrimitive
      (T.toVal param)
      (T.toVal st)
      dummyContractEnv
      parsedContracts
      logger

  return (exprLocs <> statementLocs, his)

-- | Make snapshots history for simple contract.
mkSnapshotsFor
  :: HasCallStack
  => ContractRunData -> IO (Set SourceLocation, InterpretHistory InterpretSnapshot)
mkSnapshotsFor = mkSnapshotsForImpl dummyLoggingFunction

-- | Same as @mkSnapshotsFor@ but prints
-- snapshots collection logs into the console.
{-# WARNING mkSnapshotsForLogging "'mkSnapshotsForLogging' remains in code" #-}
mkSnapshotsForLogging
  :: HasCallStack
  => ContractRunData -> IO (Set SourceLocation, InterpretHistory InterpretSnapshot)
mkSnapshotsForLogging = mkSnapshotsForImpl putStrLn

withSnapshots
  :: (Monad m)
  => (Set SourceLocation, InterpretHistory InterpretSnapshot)
  -> StateT (DebuggerState InterpretSnapshot) m a
  -> m a
withSnapshots (allLocs, his) action = do
  let st = DebuggerState
        { _dsSnapshots = playInterpretHistory his
        , _dsSources = DebugSource mempty <$> groupSourceLocations (toList allLocs)
        }
  evalStateT action st

testWithSnapshotsImpl
  :: (String -> IO ())
  -> ContractRunData
  -> StateT (DebuggerState InterpretSnapshot) IO ()
  -> Assertion
testWithSnapshotsImpl logger runData action = do
  locsAndHis <- mkSnapshotsForImpl logger runData
  withSnapshots locsAndHis action

testWithSnapshots
  :: ContractRunData
  -> StateT (DebuggerState InterpretSnapshot) IO ()
  -> Assertion
testWithSnapshots = testWithSnapshotsImpl dummyLoggingFunction

{-# WARNING testWithSnapshotsLogging "'testWithSnapshotsLogging' remains in code" #-}
testWithSnapshotsLogging
  :: ContractRunData
  -> StateT (DebuggerState InterpretSnapshot) IO ()
  -> Assertion
testWithSnapshotsLogging = testWithSnapshotsImpl putStrLn

checkSnapshot
  :: (MonadState (DebuggerState InterpretSnapshot) m, MonadIO m)
  => (InterpretSnapshot -> Assertion)
  -> m ()
checkSnapshot check = frozen curSnapshot >>= liftIO . check

unexpectedSnapshot
  :: HasCallStack => InterpretSnapshot -> Assertion
unexpectedSnapshot sp =
  HUnit.assertFailure $ "Unexpected snapshot:\n" <> pretty sp

fromValCasting :: forall a t. (T.IsoValue a, SingI t) => T.Value t -> Maybe a
fromValCasting v = do
  Refl <- sing @(T.ToT a) `decideEquality` sing @t
  return $ T.fromVal v

pattern SomeLorentzValue :: T.IsoValue v => v -> T.SomeValue
pattern SomeLorentzValue v <- T.SomeValue (fromValCasting -> Just v)
  where SomeLorentzValue v =  T.SomeValue (T.toVal v)

isPermutationOf :: (Ord a) => [a] -> [a] -> Bool
isPermutationOf xs ys = sort xs == sort ys

