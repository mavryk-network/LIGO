module RIO.Types
  ( Contract (..)
  , IndexOptions (..)
  , RioEnv (..)
  , RIO (..)

  , getCustomConfig
  ) where

import Control.Monad.IO.Unlift (MonadUnliftIO)
import Control.Monad.Reader (MonadIO, MonadReader, ReaderT, asks, mapReaderT)
import Control.Monad.Trans (lift)
import Data.Default (def)
import Data.HashSet (HashSet)
import Katip (Katip (..), KatipContext (..))
import Language.LSP.Server qualified as S
import Language.LSP.Types qualified as J
import StmContainers.Map qualified as StmMap
import UnliftIO.MVar (MVar, tryReadMVar)

import AST (ContractInfo', Includes, ParsedContractInfo)
import ASTMap (ASTMap)
import Cli (HasLigoClient (..), LigoClientEnv (..))
import Config (Config (..))
import Log (LogT)
import Log qualified

data Contract = Contract
  { cTree :: ContractInfo'
  , cDeps :: [J.NormalizedUri]
  }

-- | Represents the user's choice on how to index the project.
data IndexOptions
  = IndexChoicePending
  -- ^ The choice was not yet processed and is pending. Only the currently
  -- opened contract is indexed.
  | DoNotIndex
  -- ^ The project should not be indexed. Like when the choice is pending, only
  -- the currently opened contract is indexed.
  | FromRoot FilePath
  -- ^ Index the project starting from the root directory. That is, the
  -- directory that is currently open in Visual Studio Code, if any.
  | FromGitProject FilePath
  -- ^ Index the project from the output of `git rev-parse --show-toplevel`, if
  -- Git is set.
  | FromLigoProject FilePath
  -- ^ Index from the directory where the first `.ligoproject` file is found, if
  -- it exists. This option has precedence over all others, and if this file is
  -- present, all other options will be ignored.

-- | Stores information about the current language server environment, such as
-- loaded files, files in the project, etc. This is meant to be used inside a
-- `ReaderT`, and its internal `MVar`s updated as needed.
data RioEnv = RioEnv
  { reConfig :: MVar Config
  -- ^ Contains the current configuration of the language server, such as the
  -- path to LIGO.
  , reCache :: ASTMap J.NormalizedUri Contract RIO
  -- ^ Caches parsed and scoped contracts, as well as their include dependencies.
  -- Also contains metadata about contracts, such as when they were loaded, when
  -- they were invalidated, etc.
  , reOpenDocs :: MVar (HashSet J.NormalizedUri)
  -- ^ Records which files are current open in the editor.
  , reIncludes :: MVar (Includes ParsedContractInfo)
  -- ^ Stores the inclusion graph with respect to the currently open file.
  , reTempFiles :: StmMap.Map J.NormalizedFilePath J.NormalizedFilePath
  -- ^ Provides a way to look which temporary files correspond to which open files.
  , reIndexOpts :: MVar IndexOptions
  -- ^ Stores the user's choice (or lack of) in how the project should be indexed.
  , reBuildGraph :: MVar (Includes FilePath)
  -- ^ Represents the build graph for all files that were looked up.
  }

-- TODO: The lsp library provides no way to update the Config in the LspM monad
-- manually. So we have to maintain our own config to store the result of
-- `workspace/configuration` requests. We should get this fixed by the
-- maintainers, if possible.
getCustomConfig :: RIO Config
getCustomConfig = Log.addNamespace "getCustomConfig" do
  mConfig <- asks reConfig
  contents <- tryReadMVar mConfig
  case contents of
    -- TODO: The lsp library only sends the `workspace/configuration` request
    -- after initialization is complete, so during initialization, there is no
    -- way to know the client configuration. We need to get this fixed by the
    -- library maintainers, if possible.
    Nothing -> do
      $(Log.warning) "No config fetched yet, resorting to default"
      pure def
    Just config -> pure config

newtype RIO a = RIO
  { unRio :: ReaderT RioEnv (S.LspT Config (LogT IO)) a
  }
  deriving newtype
    ( Functor
    , Applicative
    , Monad
    , MonadIO
    , MonadReader RioEnv
    , MonadUnliftIO
    , S.MonadLsp Config.Config
    )

instance MonadFail RIO where
  fail = RIO . lift . lift . lift . fail

instance Katip RIO where
  getLogEnv = RIO $ lift $ lift getLogEnv
  localLogEnv f = RIO . mapReaderT (S.LspT . localLogEnv f . S.unLspT) . unRio

instance KatipContext RIO where
  getKatipContext = RIO $ lift $ lift getKatipContext
  localKatipContext f = RIO . mapReaderT (S.LspT . localKatipContext f . S.unLspT) . unRio
  getKatipNamespace = RIO $ lift $ lift getKatipNamespace
  localKatipNamespace f = RIO . mapReaderT (S.LspT . localKatipNamespace f . S.unLspT) . unRio

instance HasLigoClient RIO where
  getLigoClientEnv = fmap (LigoClientEnv . _cLigoBinaryPath) getCustomConfig
