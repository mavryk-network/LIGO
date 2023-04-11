-- | Helpers for implementing DAP handlers.
module Language.LIGO.Debugger.Handlers.Helpers
  ( module Language.LIGO.Debugger.Handlers.Helpers
  ) where

import Prelude hiding (try)

import AST (LIGO, Lang, insertPreprocessorRanges, nestedLIGO, parsePreprocessed)
import AST.Scope.Common qualified as AST.Common
import Cli (HasLigoClient, LigoIOException)
import Control.Concurrent.STM (throwSTM, writeTChan)
import Control.Exception (throw)
import Control.Lens (Each (each))
import Control.Monad.Except (liftEither, throwError)
import Control.Monad.STM.Class (MonadSTM (..))
import Data.Char qualified as C
import Data.HashMap.Strict qualified as HM
import Data.Singletons (SingI, demote)
import Data.Typeable (cast)
import Fmt (Buildable (..), pretty)
import Log (runNoLoggingT)
import Morley.Debugger.Core.Common (typeCheckingForDebugger)
import Morley.Debugger.Core.Navigate (SourceLocation)
import Morley.Debugger.DAP.LanguageServer qualified as MD
import Morley.Debugger.DAP.Types
  (DAPOutputMessage (..), DAPSpecificResponse (..), HandlerEnv (..),
  HasSpecificMessages (LanguageServerStateExt), RIO, RioContext (..))
import Morley.Michelson.Parser qualified as P
import Morley.Michelson.TypeCheck (typeVerifyTopLevelType)
import Morley.Michelson.Typed (Contract' (..))
import Morley.Michelson.Typed qualified as T
import Morley.Michelson.Untyped qualified as U
import Morley.Util.Constrained (Constrained (..))
import Morley.Util.Lens (makeLensesWith, postfixLFields)
import ParseTree (pathToSrc)
import Parser (ParsedInfo)
import Text.Interpolation.Nyan
import UnliftIO.Exception (fromEither, throwIO, try)

import Control.DelayedValues qualified as DelayedValues
import Language.LIGO.Debugger.CLI.Call
import Language.LIGO.Debugger.CLI.Types
import Language.LIGO.Debugger.Common
import Language.LIGO.Debugger.Error
import Language.LIGO.Debugger.Michelson

-- | Type which caches all things that we need for
-- launching the contract.
--
-- All these @Maybe@s inside this type are needed
-- to track whether this field is initialized or not.
-- If you try to unwrap @Nothing@ when @Just@ is expected
-- then a @PluginCommunicationException@ should be thrown.
data CollectedRunInfo where
  CollectedRunInfo
    :: forall cp st arg
     . (T.ParameterScope cp, T.StorageScope st)
    =>
    { criContract :: T.Contract cp st
    , criEpcMb :: Maybe (T.EntrypointCallT cp arg)
    , criParameterMb :: Maybe (T.Value arg)
    , criStorageMb :: Maybe (T.Value st)
    } -> CollectedRunInfo

onlyContractRunInfo
  :: forall cp st
   . (T.ParameterScope cp, T.StorageScope st)
  => T.Contract cp st
  -> CollectedRunInfo
onlyContractRunInfo contract = CollectedRunInfo
  { criContract = contract
  , criEpcMb = Nothing
  , criParameterMb = Nothing
  , criStorageMb = Nothing
  }

-- | The information for conversion of Michelson value to LIGO format.
data PreLigoConvertInfo = PreLigoConvertInfo
  { plciLang :: Lang
    -- ^ LIGO dialect.
  , plciMichVal :: T.SomeValue
    -- ^ Michelson value.
  , plciLigoType :: LigoType
    -- ^ Known LIGO type of the value which the conversion will result in.
  } deriving stock (Show, Eq)

instance Hashable PreLigoConvertInfo where
  hashWithSalt s (PreLigoConvertInfo lang (Constrained michVal) ty) = s
    `hashWithSalt` lang
    `hashWithSalt` pretty @(T.Value _) @Text michVal
    -- ↑ Pretty-printer for michelson values on itself is not a reversible
    -- function (e.g. address and contract are represented in the same way),
    -- but if we include type, the resulting values will be unique
    `hashWithSalt` ty

-- | LIGO-debugger-specific state that we initialize before debugger session
-- creation.
data LigoLanguageServerState = LigoLanguageServerState
  { lsProgram :: Maybe FilePath
  , lsCollectedRunInfo :: Maybe CollectedRunInfo
  , lsEntrypoint :: Maybe String  -- ^ @main@ method to use
  , lsAllLocs :: Maybe (Set SourceLocation)
  , lsBinaryPath :: Maybe FilePath
  , lsParsedContracts :: Maybe (HashMap FilePath (LIGO ParsedInfo))
  , lsLambdaLocs :: Maybe (HashSet LigoRange)
  , lsToLigoValueConverter :: DelayedValues.Manager PreLigoConvertInfo Text
  }

instance Buildable LigoLanguageServerState where
  build LigoLanguageServerState{..} = [int||
    Debugging program: #{lsProgram}
    |]

writeResponse :: DAPSpecificResponse ext -> RIO ext ()
writeResponse msg = do
  ch <- asks _rcOutputChannel
  atomically $ writeTChan ch (DAPResponse msg)

withMichelsonEntrypoint
  :: (MonadIO m)
  => T.Contract param st
  -> Maybe String
  -> (forall arg. SingI arg => T.Notes arg -> T.EntrypointCallT param arg -> m a)
  -> m a
withMichelsonEntrypoint contract@T.Contract{} mEntrypoint cont = do
  let noParseEntrypointErr = ConfigurationException .
        [int|m|Could not parse entrypoint: #{id}|]
  michelsonEntrypoint <- case mEntrypoint of
    Nothing -> pure U.DefEpName
    -- extension may return default entrypoints as "default"
    Just "default" -> pure U.DefEpName
    Just ep -> U.buildEpName (toText $ firstLetterToLowerCase ep)
      & first noParseEntrypointErr
      & fromEither

  let noEntrypointErr = ConfigurationException
        [int||Entrypoint `#{michelsonEntrypoint}` not found|]
  T.MkEntrypointCallRes notes call <-
    T.mkEntrypointCall michelsonEntrypoint (cParamNotes contract)
    & maybe (throwIO noEntrypointErr) pure

  cont notes call
  where
    -- LIGO has constructors starting from capital letters,
    -- however in Michelson they appear as field annotations starting from
    -- lower-case letter.
    -- This is a detail that we would like to hide from the end user.
    firstLetterToLowerCase = \case
      [] -> []
      c : rest -> C.toLower c : rest

-- | Try our best to parse and typecheck a value of a certain category.
parseValue
  :: forall t m.
     (SingI t, HasLigoClient m)
  => FilePath
  -> Text
  -> Text
  -> Text
  -> m (Either Text (T.Value t))
parseValue ctxContractPath category val valueLang = runExceptT do
  let src = P.MSName category
  uvalue <- case valueLang of
    "LIGO" -> do
      lift (try $ compileLigoExpression src ctxContractPath val) >>= \case
        Right x -> pure x
        Left (err :: LigoCallException) -> throwError [int||
            Error parsing #{category}:

            #{err}
          |]
    "Michelson" ->
      P.parseExpandValue src val
        & first (pretty . MD.prettyFirstError)
        & liftEither

    _ -> throwError [int||
        Expected "LIGO" or "Michelson" in field "#{category}Lang" \
        but got #{valueLang}
      |]

  typeVerifyTopLevelType mempty uvalue
    & typeCheckingForDebugger
    & first do \_ -> [int||
        The value is not of type `#{demote @t}`
      |]
      -- TODO [LIGO-913]: mention LIGO type
    & liftEither

getServerState :: RIO ext (LanguageServerStateExt ext)
getServerState =
  asks _rcLSState >>= readTVarIO >>=
    maybe (throwIO uninitLanguageServerExc) pure

-- | Get server state in handler's context
getServerStateH :: (MonadReader (HandlerEnv ext) m, MonadSTM m) => m (LanguageServerStateExt ext)
getServerStateH =
  asks heLSState >>= liftSTM . readTVar >>=
    maybe (liftSTM $ throwSTM uninitLanguageServerExc) pure

uninitLanguageServerExc :: PluginCommunicationException
uninitLanguageServerExc =
  PluginCommunicationException "Language server state is not initialized"

expectInitialized :: (MonadIO m) => Text -> m (Maybe a) -> m a
expectInitialized errMsg maybeM = maybeM >>= \case
  Nothing -> throwIO $ PluginCommunicationException errMsg
  Just val -> pure val

getProgram
  :: (LanguageServerStateExt ext ~ LigoLanguageServerState)
  => RIO ext FilePath
getProgram = "Program is not initialized" `expectInitialized` (lsProgram <$> getServerState)

getCollectedRunInfo
  :: (LanguageServerStateExt ext ~ LigoLanguageServerState)
  => RIO ext CollectedRunInfo
getCollectedRunInfo =
  "Collected run info is not initialized" `expectInitialized` (lsCollectedRunInfo <$> getServerState)

getAllLocs
  :: (LanguageServerStateExt ext ~ LigoLanguageServerState)
  => RIO ext (Set SourceLocation)
getAllLocs = "All locs are not initialized" `expectInitialized` (lsAllLocs <$> getServerState)

getParsedContracts
  :: (LanguageServerStateExt ext ~ LigoLanguageServerState)
  => RIO ext (HashMap FilePath (LIGO ParsedInfo))
getParsedContracts =
  "Parsed contracts are not initialized" `expectInitialized` (lsParsedContracts <$> getServerState)

getLambdaLocs
  :: (LanguageServerStateExt ext ~ LigoLanguageServerState)
  => RIO ext (HashSet LigoRange)
getLambdaLocs = "Lambda locs are not initialized" `expectInitialized` (lsLambdaLocs <$> getServerState)

parseContracts :: (HasLigoClient m) => [FilePath] -> m (HashMap FilePath (LIGO ParsedInfo))
parseContracts allFiles = do
  parsedInfos <- runNoLoggingT do
    forM allFiles
      $   pathToSrc
          -- This shouldn't happen because vscode saves all files before debugging.
          -- Also, @pathToSrc@ reads file from a disk and sets @isDirty = false@.
          -- So, we have another layer of safety.
      >=> parsePreprocessed (throw $ ImpossibleHappened "Debugging started on dirty file")
      >=> insertPreprocessorRanges

  let parsedFiles = parsedInfos ^.. each . AST.Common.getContract . AST.Common.cTree . nestedLIGO

  pure $ HM.fromList $ zip allFiles parsedFiles

-- | Some exception in debugger logic.
data SomeDebuggerException where
  SomeDebuggerException :: DebuggerException e => e -> SomeDebuggerException

deriving stock instance Show SomeDebuggerException

instance Exception SomeDebuggerException where
  displayException (SomeDebuggerException e) = displayException e

  fromException e@(SomeException e') =
    asum
      [ SomeDebuggerException <$> fromException @LigoCallException e
      , SomeDebuggerException <$> fromException @LigoDecodeException e
      , SomeDebuggerException <$> fromException @MichelsonDecodeException e
      , SomeDebuggerException <$> fromException @ConfigurationException e
      , SomeDebuggerException <$> fromException @UnsupportedLigoVersionException e
      , SomeDebuggerException <$> fromException @ReplacementException e
      , SomeDebuggerException <$> fromException @PluginCommunicationException e
      , SomeDebuggerException <$> fromException @ImpossibleHappened e
      , SomeDebuggerException <$> fromException @LigoIOException e
      , cast @_ @SomeDebuggerException e'
      ]

makeLensesWith postfixLFields ''LigoLanguageServerState
