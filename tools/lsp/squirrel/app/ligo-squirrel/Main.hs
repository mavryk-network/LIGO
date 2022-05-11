{-# LANGUAGE PolyKinds #-}

module Main (main) where

import Colog.Core qualified as Colog
import Control.Lens hiding ((:>))
import Control.Monad.IO.Class (liftIO)
import Control.Monad.Reader (asks, void, when)
import Data.Aeson qualified as Aeson
import Data.Bool (bool)
import Data.Default
import Data.Foldable (for_)
import Data.HashSet qualified as HashSet
import Data.Maybe (fromMaybe)
import Data.Set qualified as Set
import Data.Text qualified as T
import Language.LSP.Logging as L
import Language.LSP.Server qualified as S
import Language.LSP.Types qualified as J
import Language.LSP.Types.Lens qualified as J
import Prettyprinter qualified as PP
import System.Exit
import System.IO (stdin, stdout)
import UnliftIO.Exception (SomeException (..), displayException, withException)
import UnliftIO.MVar (modifyMVar_, tryReadMVar)

import AST
import Cli.Impl (getLigoVersion)
import Config (Config (..))
import Extension (isLigoFile)
import Language.LSP.Util (sendError)
import Log (i)
import Log qualified
import RIO (RIO, RioEnv (..))
import RIO qualified
import RIO.Diagnostic qualified as Diagnostic
import RIO.Document qualified as Document
import RIO.Indexing qualified as Indexing
import Range
import Util (toLocation)

main :: IO ()
main = exit =<< mainLoop

mainLoop :: IO Int
mainLoop =
  Log.withLogger $$(Log.flagBasedSeverity) "lls" $$(Log.flagBasedEnv) \runLogger -> do
    let
      serverDefinition = S.ServerDefinition
        { S.onConfigurationChange = \old _ -> Right old
        , S.defaultConfig = def
        , S.doInitialize = \lcEnv _msg -> Right . (lcEnv, ) <$> RIO.newRioEnv
        , S.staticHandlers = catchExceptions handlers
        , S.interpretHandler = \envs -> S.Iso (runLogger . RIO.run envs) liftIO
        , S.options = lspOptions
        }

    S.runServerWithHandles mempty lspLogger stdin stdout serverDefinition
  where
    syncOptions :: J.TextDocumentSyncOptions
    syncOptions = J.TextDocumentSyncOptions
      { J._openClose         = Just True
      , J._change            = Just J.TdSyncIncremental
      , J._willSave          = Just False
      , J._willSaveWaitUntil = Just False
      , J._save              = Just $ J.InR $ J.SaveOptions $ Just False
      }

    lspOptions :: S.Options
    lspOptions = def
      { S.textDocumentSync = Just syncOptions
      , S.signatureHelpTriggerCharacters = Just ['(', ' ']
      , S.signatureHelpRetriggerCharacters = Just [',']
      }

    -- | Show a error message to the user if an exception crashes the server.
    -- The LSP protocol defines that the client should handle server crashes and
    -- attempt to reasonably restart it. For example, Visual Studio Code will
    -- attempt to restart the server 5 times within 3 minutes, and will leave it
    -- dead if it continues crashing within that time frame.
    catchExceptions :: S.Handlers RIO -> S.Handlers RIO
    catchExceptions = S.mapHandlers
      (wrapReq . handleDisabledReq . addReqLogging)
      (wrapNotif . addNotifLogging)
      where
        wrapReq
          :: forall (meth :: J.Method 'J.FromClient 'J.Request).
             S.Handler RIO meth -> S.Handler RIO meth
        wrapReq handler msg@J.RequestMessage{_method} resp = Log.addNamespace "wrapReq" $
          handler msg resp `withException` \(SomeException e) -> do
            $(Log.critical) [i|Handling `#{_method}`: #{displayException e}|]
            resp . Left $ J.ResponseError J.InternalError (T.pack $ displayException e) Nothing

        wrapNotif
          :: forall (meth :: J.Method 'J.FromClient 'J.Notification).
             S.Handler RIO meth -> S.Handler RIO meth
        wrapNotif handler msg@J.NotificationMessage{_method} = Log.addNamespace "wrapNotif" $
          handler msg `withException` \(SomeException e) -> do
            $(Log.critical) [i|Handling `#{_method}`: #{displayException e}|]
            sendError . T.pack $ "Error handling `" <> show _method <> "` (see logs)."

        addReqLogging
          :: forall (meth :: J.Method 'J.FromClient 'J.Request).
             S.Handler RIO meth -> S.Handler RIO meth
        addReqLogging handler msg@J.RequestMessage{_method} resp = Log.addNamespace [i|#{_method}|] do
          version <- getLigoVersion
          maybe id Log.addContext version $ handler msg resp

        addNotifLogging
          :: forall (meth :: J.Method 'J.FromClient 'J.Notification).
             S.Handler RIO meth -> S.Handler RIO meth
        addNotifLogging handler msg@J.NotificationMessage{_method} = Log.addNamespace [i|#{_method}|] do
          version <- getLigoVersion
          maybe id Log.addContext version $ handler msg

        handleDisabledReq
          :: forall (meth :: J.Method 'J.FromClient 'J.Request).
             S.Handler RIO meth -> S.Handler RIO meth
        handleDisabledReq handler msg@J.RequestMessage{_method} resp = do
          Config {_cDisabledFeatures} <- S.getConfig
          let err = T.pack [i|Cannot handle #{_method}: disabled by user.|]
          if Set.member (J.SomeClientMethod _method) _cDisabledFeatures
            then resp $ Left $ J.ResponseError J.RequestCancelled err Nothing
            else handler msg resp

    lspLogger :: Colog.LogAction (S.LspM Config) (Colog.WithSeverity S.LspServerLog)
    lspLogger =
      Colog.filterBySeverity Colog.Error Colog.getSeverity
      $ Colog.cmap (fmap (T.pack . show . PP.pretty)) L.logToLogMessage

handlers :: S.Handlers RIO
handlers = mconcat
  [ S.notificationHandler J.SInitialized handleInitialized

  , S.notificationHandler J.STextDocumentDidOpen handleDidOpenTextDocument
  , S.notificationHandler J.STextDocumentDidChange handleDidChangeTextDocument
  , S.notificationHandler J.STextDocumentDidSave (\_msg -> pure ())
  , S.notificationHandler J.STextDocumentDidClose handleDidCloseTextDocument

  , S.requestHandler J.STextDocumentDefinition handleDefinitionRequest
  , S.requestHandler J.STextDocumentTypeDefinition handleTypeDefinitionRequest
  , S.requestHandler J.STextDocumentReferences handleFindReferencesRequest
  , S.requestHandler J.STextDocumentCompletion handleCompletionRequest
  , S.requestHandler J.STextDocumentSignatureHelp handleSignatureHelpRequest
  , S.requestHandler J.STextDocumentFoldingRange handleFoldingRangeRequest
  , S.requestHandler J.STextDocumentSelectionRange handleSelectionRangeRequest
  , S.requestHandler J.STextDocumentDocumentLink handleDocumentLinkRequest
  , S.requestHandler J.STextDocumentDocumentSymbol handleDocumentSymbolsRequest
  , S.requestHandler J.STextDocumentHover handleHoverRequest
  , S.requestHandler J.STextDocumentRename handleRenameRequest
  , S.requestHandler J.STextDocumentPrepareRename handlePrepareRenameRequest
  , S.requestHandler J.STextDocumentFormatting handleDocumentFormattingRequest
  , S.requestHandler J.STextDocumentRangeFormatting handleDocumentRangeFormattingRequest
  , S.requestHandler J.STextDocumentCodeAction handleTextDocumentCodeAction

  , S.notificationHandler J.SCancelRequest (\_msg -> pure ())
  , S.notificationHandler J.SWorkspaceDidChangeConfiguration handleDidChangeConfiguration
  , S.notificationHandler J.SWorkspaceDidChangeWatchedFiles handleDidChangeWatchedFiles

  , S.requestHandler (J.SCustomMethod "buildGraph") handleCustomMethod'BuildGraph
  ]

handleInitialized :: S.Handler RIO 'J.Initialized
handleInitialized _ = RIO.initializeRio

handleDidOpenTextDocument :: S.Handler RIO 'J.TextDocumentDidOpen
handleDidOpenTextDocument notif = do
  let uri = notif^.J.params.J.textDocument.J.uri.to J.toNormalizedUri
  let ver = notif^.J.params.J.textDocument.J.version

  RIO.Contract doc _ <- Document.forceFetch' Document.BestEffort uri
  openDocsVar <- asks reOpenDocs
  modifyMVar_ openDocsVar \openDocs -> do
    Diagnostic.collectErrors doc (Just ver)
    pure $ HashSet.insert uri openDocs

handleDidChangeTextDocument :: S.Handler RIO 'J.TextDocumentDidChange
handleDidChangeTextDocument notif = do
  let uri = notif^.J.params.J.textDocument.J.uri.to J.toNormalizedUri
  $(Log.debug) [i|Changed text document: #{uri}|]
  void $ Document.forceFetchAndNotify notify Document.LeastEffort uri
  where
    -- Clear diagnostics for all contracts in this WCC and then send diagnostics
    -- collected from this URI.
    -- The usage of `openDocsVar` here serves purely as a mutex to prevent race
    -- conditions.
    notify :: RIO.Contract -> RIO ()
    notify (RIO.Contract doc nuris) = do
      let ver = notif^.J.params.J.textDocument.J.version
      openDocsVar <- asks reOpenDocs
      modifyMVar_ openDocsVar \openDocs -> do
        Diagnostic.clearDiagnostics nuris
        Diagnostic.collectErrors doc ver
        pure openDocs

handleDidCloseTextDocument :: S.Handler RIO 'J.TextDocumentDidClose
handleDidCloseTextDocument notif = do
  let uri = notif^.J.params.J.textDocument.J.uri.to J.toNormalizedUri

  RIO.Contract _ nuris <- Document.fetch' Document.LeastEffort uri

  openDocsVar <- asks reOpenDocs
  modifyMVar_ openDocsVar \openDocs -> do
    let openDocs' = HashSet.delete uri openDocs
    -- Clear diagnostics for all contracts in this WCC group if all of them were closed.
    let nuriMap = HashSet.fromList nuris
    when (HashSet.null $ HashSet.intersection openDocs' nuriMap) $
      Diagnostic.clearDiagnostics nuris
    pure openDocs'

handleDefinitionRequest :: S.Handler RIO 'J.TextDocumentDefinition
handleDefinitionRequest req respond = do
    -- XXX: They forgot lenses for DefinitionParams :/
    {-
    let uri = req^.J.textDocument.J.uri
    let pos = fromLspPosition $ req^.J.position
    -}
    let
      J.DefinitionParams{_textDocument, _position} = req ^. J.params
      uri = _textDocument ^. J.uri
      pos = fromLspPosition _position
    tree <- contractTree <$> Document.fetch Document.LeastEffort (J.toNormalizedUri uri)
    let location = case AST.definitionOf pos tree of
          Just defPos -> [toLocation defPos]
          Nothing     -> []
    $(Log.debug) [i|Definition request returned #{location}|]
    respond . Right . J.InR . J.InL . J.List $ location

handleTypeDefinitionRequest :: S.Handler RIO 'J.TextDocumentTypeDefinition
handleTypeDefinitionRequest req respond = do
    let
      J.TypeDefinitionParams{_textDocument, _position} = req ^. J.params
      uri = _textDocument ^. J.uri
      pos = _position ^. to fromLspPosition
    tree <- contractTree <$> Document.fetch Document.LeastEffort (J.toNormalizedUri uri)
    let wrapAndRespond = respond . Right . J.InR . J.InL . J.List
    let definition = case AST.typeDefinitionAt pos tree of
          Just defPos -> [J.Location uri $ toLspRange defPos]
          Nothing     -> []
    $(Log.debug) [i|Type definition request returned #{definition}|]
    wrapAndRespond definition

handleDocumentFormattingRequest :: S.Handler RIO 'J.TextDocumentFormatting
handleDocumentFormattingRequest req respond = do
  let
    uri = req ^. J.params . J.textDocument . J.uri
    nuri = J.toNormalizedUri uri
  tree <- contractTree <$> Document.fetch Document.BestEffort nuri
  Document.invalidate nuri
  respond . Right =<< AST.formatDocument tree

handleDocumentRangeFormattingRequest :: S.Handler RIO 'J.TextDocumentRangeFormatting
handleDocumentRangeFormattingRequest req respond = do
  let
    uri = req ^. J.params . J.textDocument . J.uri
    nuri = J.toNormalizedUri uri
    pos = fromLspRange $ req ^. J.params . J.range
  tree <- contractTree <$> Document.fetch Document.BestEffort nuri
  Document.invalidate nuri
  respond . Right =<< AST.formatAt pos tree

handleFindReferencesRequest :: S.Handler RIO 'J.TextDocumentReferences
handleFindReferencesRequest req respond = do
    let (_, nuri, pos) = getUriPos req
    tree <- contractTree <$> Document.fetch Document.NormalEffort nuri
    let locations = case AST.referencesOf pos tree of
          Just refs -> toLocation <$> refs
          Nothing   -> []
    $(Log.debug) [i|Find references request returned #{locations}|]
    respond . Right . J.List $ locations

handleCompletionRequest :: S.Handler RIO 'J.TextDocumentCompletion
handleCompletionRequest req respond = do
    let uri = req ^. J.params . J.textDocument . J.uri . to J.toNormalizedUri
    let pos = fromLspPosition $ req ^. J.params . J.position
    tree <- contractTree <$> Document.fetch Document.LeastEffort uri
    let completions = fmap toCompletionItem . fromMaybe [] $ complete pos tree
    $(Log.debug) [i|Completion request returned #{completions}|]
    respond . Right . J.InL . J.List $ completions

handleSignatureHelpRequest :: S.Handler RIO 'J.TextDocumentSignatureHelp
handleSignatureHelpRequest req respond = do
  -- XXX: They forgot lenses for  SignatureHelpParams :/
  {-
  let uri = req ^. J.params . J.textDocument . J.uri
  let position = req ^. J.params . J.position & fromLspPosition
  -}
  let
    J.SignatureHelpParams{_textDocument, _position} = req ^. J.params
    uri = _textDocument ^. J.uri
    position = fromLspPosition _position
  tree <- contractTree <$> Document.fetch Document.LeastEffort (J.toNormalizedUri uri)
  let signatureHelp = getSignatureHelp (tree ^. nestedLIGO) position
  respond . Right $ signatureHelp

handleFoldingRangeRequest :: S.Handler RIO 'J.TextDocumentFoldingRange
handleFoldingRangeRequest req respond = do
    let uri = req ^. J.params . J.textDocument . J.uri . to J.toNormalizedUri
    tree <- contractTree <$> Document.fetch Document.LeastEffort uri
    let actions = foldingAST (tree ^. nestedLIGO)
    respond . Right . J.List $ toFoldingRange <$> actions

handleTextDocumentCodeAction :: S.Handler RIO 'J.TextDocumentCodeAction
handleTextDocumentCodeAction req respond = do
    let
      uri = req ^. J.params . J.textDocument . J.uri . to J.toNormalizedUri
      r = req ^. J.params . J.range . to fromLspRange
      con = req ^. J.params . J.context
    tree <- contractTree <$> Document.fetch Document.LeastEffort uri
    let actions = collectCodeActions r con (J.fromNormalizedUri uri) tree
    let response = Right . J.List . fmap J.InR $ actions
    respond response

handleSelectionRangeRequest :: S.Handler RIO 'J.TextDocumentSelectionRange
handleSelectionRangeRequest req respond = do
    let uri = req ^. J.params . J.textDocument . J.uri . to J.toNormalizedUri
    let positions = req ^. J.params . J.positions ^.. folded
    tree <- contractTree <$> Document.fetch Document.NormalEffort uri
    let results = map (findSelectionRange (tree ^. nestedLIGO)) positions
    respond . Right . J.List $ results

handleDocumentLinkRequest :: S.Handler RIO 'J.TextDocumentDocumentLink
handleDocumentLinkRequest req respond = do
  let uri = req ^. J.params . J.textDocument . J.uri . to J.toNormalizedUri
  contractInfo <- Document.fetch Document.LeastEffort uri
  collected <-
    getDocumentLinks
      (contractFile contractInfo)
      (getLIGO (contractTree contractInfo))
  respond . Right . J.List $ collected

handleDocumentSymbolsRequest :: S.Handler RIO 'J.TextDocumentDocumentSymbol
handleDocumentSymbolsRequest req respond = do
    let uri = req ^. J.params . J.textDocument . J.uri . to J.toNormalizedUri
    tree <- contractTree <$> Document.fetch Document.LeastEffort uri
    let result = extractDocumentSymbols (J.fromNormalizedUri uri) tree
    respond . Right . J.InR . J.List $ result

handleHoverRequest :: S.Handler RIO 'J.TextDocumentHover
handleHoverRequest req respond = do
    -- XXX: They forgot lenses for  HoverParams :/
    {-
    let uri = req ^. J.params . J.textDocument . J.uri . to J.toNormalizedUri
    let pos = fromLspPosition $ req ^. J.params . J.position
    -}
    let
      J.HoverParams{_textDocument, _position} = req ^. J.params
      uri = _textDocument ^. J.uri . to J.toNormalizedUri
      pos = fromLspPosition _position
    tree <- contractTree <$> Document.fetch Document.LeastEffort uri
    respond . Right $ hoverDecl pos tree

handleRenameRequest :: S.Handler RIO 'J.TextDocumentRename
handleRenameRequest req respond = do
    let (_, nuri, pos) = getUriPos req
    let newName = req ^. J.params . J.newName

    tree <- contractTree <$> Document.fetch Document.NormalEffort nuri

    case renameDeclarationAt pos tree newName of
      NotFound -> do
        $(Log.debug) [i|Declaration not found for: #{show req}|]
        respond . Left $
          J.ResponseError J.InvalidRequest "Cannot rename this" Nothing
      Ok edits -> do
        let
          -- XXX: This interface has two benefits: it allows to refer to a specific
          -- document version and it allows the creation/deletion/renaming of files.
          -- In this case we do not care about the latter and the actual usefulness
          -- of the former is not clear either, but it might be worth switching
          -- to it when we support versions.
          --documentChanges = J.List
          --  [ J.TextDocumentEdit
          --      { _textDocument = J.VersionedTextDocumentIdentifier uri Nothing
          --      , _edits = J.List edits
          --      }
          --  ]

          response =
            J.WorkspaceEdit
              { _changes = Just edits
              , _documentChanges = Nothing
              , _changeAnnotations = Nothing
              }
        Document.invalidate nuri
        respond . Right $ response

handlePrepareRenameRequest :: S.Handler RIO 'J.TextDocumentPrepareRename
handlePrepareRenameRequest req respond = do
    let (_, nuri, pos) = getUriPos req

    tree <- contractTree <$> Document.fetch Document.NormalEffort nuri

    respond . Right . fmap (J.InL . toLspRange) $ prepareRenameDeclarationAt pos tree

handleDidChangeConfiguration :: S.Handler RIO 'J.WorkspaceDidChangeConfiguration
handleDidChangeConfiguration notif = do
  let value = notif ^. J.params . J.settings
   in case value of
        Aeson.Null -> RIO.fetchConfig
        _ -> RIO.setConfigFromJSON value

handleDidChangeWatchedFiles :: S.Handler RIO 'J.WorkspaceDidChangeWatchedFiles
handleDidChangeWatchedFiles notif = do
  let J.List changes = notif ^. J.params . J.changes
  for_ changes \(J.FileEvent (J.toNormalizedUri -> uri) change) ->
    for_ (J.uriToNormalizedFilePath uri) \nfp -> do
      let fp = J.fromNormalizedFilePath nfp
      bool Indexing.handleProjectFileChanged Document.handleLigoFileChanged (isLigoFile fp) nfp change

handleCustomMethod'BuildGraph
  :: S.Handler RIO ('J.CustomMethod :: J.Method 'J.FromClient 'J.Request)
handleCustomMethod'BuildGraph req respond =
  case req ^. J.params of
    Aeson.Null -> do
      buildGraphM <- tryReadMVar =<< asks reBuildGraph
      respond $ Right $ maybe Aeson.Null Aeson.toJSON buildGraphM
    _ ->
      respond $ Left $ J.ResponseError J.InvalidRequest "This request expects null" Nothing

getUriPos
  :: ( J.HasPosition (J.MessageParams m) J.Position
     , J.HasUri a J.Uri
     , J.HasTextDocument (J.MessageParams m) a
     )
  => J.RequestMessage m
  -> (J.Uri, J.NormalizedUri, Range)
getUriPos req =
  let
    uri  = req ^. J.params . J.textDocument . J.uri
    nuri = J.toNormalizedUri uri
    pos  = fromLspPositionUri (req ^. J.params . J.position) uri
  in (uri, nuri, pos)

exit :: Int -> IO ()
exit 0 = exitSuccess
exit n = exitWith (ExitFailure n)
