module LSP.ReceiveData (receiveData) where

import Control.Arrow ((>>>))
import Control.Lens (to)
import Data.Aeson qualified as Aeson
import Data.ByteString qualified as BS
import Data.ByteString.Lazy qualified as BSL
import Data.Map qualified as Map
import Data.Text qualified as Text
import Data.Text.Encoding qualified as Text (decodeUtf8, encodeUtf8)
import Data.Text.Lazy.Builder qualified as Text (fromText)
import Katip (LogStr(..), Severity(DebugS, WarningS), logFM)
import Language.LSP.Types (Method(..), NotificationMessage, Uri(..))
import Language.LSP.Types.Lens (params, text, textDocument, uri)
import Network.WebSockets (Connection)
import Network.WebSockets qualified as WS
import Prelude hiding (Key)
import System.Directory (createDirectoryIfMissing)
import System.IO (hFlush)

import Common (ConnectionM)
import Config (ConnectionConfig(..), ServerConfig(..))
import LSP.FilePath (modifyUri)

receiveData :: Int -> Connection -> Handle -> ConnectionM ()
receiveData clientId conn stdinProducer = do
  workspacePrefix <- asks (scLSPWorkspacePrefix . ccServerConfig)
  let prefix = workspacePrefix <> "/connection" <> show clientId <> "/"
  receiveFileTree prefix conn
  forever $ do
    msg <- liftIO (WS.receiveData conn)
    logFM DebugS ("Received: " <> LogStr (Text.fromText (prettyJSON msg)))
    let prefixed = addPrefix prefix msg
    createMissingFiles prefixed
    logFM DebugS "Created missing files"
    liftIO (hPutStr stdinProducer (Text.decodeUtf8 (addContentLength prefixed)))
    liftIO (hFlush stdinProducer)

receiveFileTree :: Text -> Connection -> ConnectionM ()
receiveFileTree prefix conn =
  let action = do
        logFM DebugS "Receiving file tree"
        count :: Int <- MaybeT (Aeson.decodeStrict <$> liftIO (WS.receiveData conn))
        forM_ [(1::Int) .. count] $ \_ -> do
          fileInfo :: Map Text Text <-
            MaybeT (Aeson.decodeStrict <$> liftIO (WS.receiveData conn))
          path <- hoistMaybe (Map.lookup "path" fileInfo)
          content <- hoistMaybe (Map.lookup "content" fileInfo)
          logFM DebugS (LogStr . Text.fromText $ path)
          lift $ writeFileWithParent (prefix <> path) content
        logFM DebugS "Finished receiving file tree"
   in runMaybeT action >>= \case
        Nothing -> logFM WarningS "Couldn't decode initial filetree"
        Just () -> pure ()

prettyJSON :: BS.ByteString -> Text
prettyJSON bs =
  case Aeson.decodeStrict bs of
    Just (obj :: Aeson.Value) ->
      decodeUtf8 (BSL.toStrict (Aeson.encode obj))
    Nothing -> "<no parse>"

addContentLength :: BS.ByteString -> BS.ByteString
addContentLength msg =
  let len = BS.length msg
      header =
        Text.encodeUtf8 . Text.pack
        $ "Content-Length: " ++ show len ++ "\r\n\r\n"
   in BS.append header msg

addPrefix :: Text -> BS.ByteString -> BS.ByteString
addPrefix clientPrefix bs =
  case Aeson.decodeStrict bs of
    Just (val :: Aeson.Value) ->
      BSL.toStrict . Aeson.encode . modifyUri (addPrefixText clientPrefix) $ val
    Nothing -> bs

addPrefixText :: Text -> Text -> Text
addPrefixText prefix txt =
  if Text.take 7 txt == "file://"
  then "file://" <> prefix <> Text.drop 7 txt
  else error $ "couldn't parse " <> txt

createMissingFiles :: BS.ByteString -> ConnectionM ()
createMissingFiles bs =
  case bs of
    (Aeson.decodeStrict @(NotificationMessage 'TextDocumentDidOpen) -> Just rm) -> do
      let filename = rm ^. (params . textDocument . uri . to getUri)
                   & Text.drop 7
          contents = rm ^. (params . textDocument . text)
      liftIO (createDirectoryIfMissing True (Text.unpack (getDir filename)))
      writeFile (Text.unpack filename) contents
    _ -> pure ()

writeFileWithParent :: Text -> Text -> ConnectionM ()
writeFileWithParent filename content = do
  liftIO (createDirectoryIfMissing True (Text.unpack (getDir filename)))
  writeFile (Text.unpack filename) content

getDir :: Text -> Text
getDir =
  Text.reverse
  >>> Text.dropWhile (/= '/')
  >>> Text.reverse
