module Lib
  ( startApp
  , mkApp
  , CompileRequest (..)
  , Config (..)
  , Source (..)
  )
where

import Control.Monad (forM_)
import Control.Monad.Except (ExceptT)
import Control.Monad.IO.Class (liftIO)
import Control.Monad.Reader (ReaderT, asks, runReaderT)
import Control.Monad.Trans (lift)
import Data.Aeson
  (FromJSON, ToJSON, defaultOptions, fieldLabelModifier, genericParseJSON, genericToJSON, parseJSON,
  toJSON)
import Data.Char (toLower)
import Data.Proxy (Proxy(Proxy))
import Data.Swagger.ParamSchema (ToParamSchema)
import Data.Swagger.Schema (ToSchema)
import Data.Text (Text)
import Data.Text qualified as Text
import Data.Text.IO qualified as Text
import GHC.Generics (Generic)
import Katip (Environment(..), KatipT, initLogEnv, runKatipT)
import Network.Wai (Middleware)
import Network.Wai.Handler.Warp (run)
import Network.Wai.Middleware.Cors (cors, corsRequestHeaders, simpleCorsResourcePolicy)
import Network.Wai.Middleware.RequestLogger (logStdoutDev)
import Servant
  (Application, Handler(..), JSON, Post, ReqBody, Server, ServerError, hoistServer, serve,
  (:<|>)((:<|>)), (:>))
import Servant.Swagger (toSwagger)
import Servant.Swagger.UI (SwaggerSchemaUI, swaggerSchemaUIServer)
import System.Directory (createDirectoryIfMissing)
import System.Exit (ExitCode(ExitFailure, ExitSuccess))
import System.FilePath (takeDirectory, (</>))
import System.IO.Temp (withSystemTempDirectory)
import System.Process (readProcessWithExitCode)

newtype Source = Source {unSource :: Text}
  deriving stock (Eq, Show, Ord, Generic)

instance FromJSON Source where
  parseJSON = genericParseJSON defaultOptions

instance ToJSON Source where
  toJSON = genericToJSON defaultOptions

instance ToSchema Source

data CompileRequest = CompileRequest
  { rSources :: [(FilePath, Source)]
  , rMain :: FilePath
  } deriving stock (Eq, Show, Ord, Generic)

prepareField :: Int -> String -> String
prepareField n = lowercaseInitial . drop n
  where
    lowercaseInitial :: String -> String
    lowercaseInitial [] = []
    lowercaseInitial (c:s) = toLower c : s

instance FromJSON CompileRequest where
  parseJSON = genericParseJSON
    defaultOptions {fieldLabelModifier = prepareField 1}

instance ToJSON CompileRequest where
  toJSON = genericToJSON defaultOptions {fieldLabelModifier = prepareField 1}

instance ToSchema CompileRequest

newtype Build = Build Text deriving stock (Show, Generic)

instance ToJSON Build where
  toJSON = genericToJSON defaultOptions

instance ToSchema Build

instance ToParamSchema Build

type API = "compile" :> ReqBody '[JSON] CompileRequest :> Post '[JSON] Build

type SwaggeredAPI =
  SwaggerSchemaUI "swagger-ui" "swagger.json"
    :<|> API

data Config = Config
  { cLigoPath :: FilePath
  , cPort :: Int
  , cVerbose :: Bool
  }

type WebIDEM = KatipT (ReaderT Config (ExceptT ServerError IO))

startApp :: Config -> IO ()
startApp config = run (cPort config) (mkApp config)

mkApp :: Config -> Application
mkApp config =
  maybeLogRequests . corsWithContentType $ serve (Proxy @SwaggeredAPI) server
  where
    maybeLogRequests :: Middleware
    maybeLogRequests =
      if cVerbose config
      then logStdoutDev
      else id

    -- Allow Content-Type header with values other then allowed by simpleCors.
    corsWithContentType :: Middleware
    corsWithContentType = cors (const $ Just policy)
      where
        policy = simpleCorsResourcePolicy
          {corsRequestHeaders = ["Content-Type"]}

    server :: Server SwaggeredAPI
    server =
      swaggerSchemaUIServer (toSwagger (Proxy @API))
        :<|> hoistServer (Proxy @API) hoist compile

    hoist :: WebIDEM a -> Handler a
    hoist x = Handler $ do
      logEnv <- liftIO $ initLogEnv "ligo-webide" (Environment "devel")
      runReaderT (runKatipT logEnv x) config

compile :: CompileRequest -> WebIDEM Build
compile request =
  let (filepaths, sources) = unzip (rSources request)
   in withSystemTempDirectory "" $ \dirPath -> do
        let fullFilepaths = map (dirPath </>) filepaths
        let fullMainPath = dirPath </> rMain request

        liftIO . forM_ (zip fullFilepaths sources) $ \(fp, src) -> do
          createDirectoryIfMissing True (takeDirectory fp)
          Text.writeFile fp (unSource src)

        ligoPath <- lift (asks cLigoPath)
        (ec, out, err) <- liftIO $
          readProcessWithExitCode
            ligoPath
            ["compile", "contract", fullMainPath]
            ""

        case ec of
          ExitSuccess -> pure (Build $ Text.pack out)
          ExitFailure _ -> pure (Build $ Text.pack err)
