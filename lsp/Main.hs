{-# LANGUAGE BlockArguments #-}
{-# LANGUAGE DataKinds #-}
{-# LANGUAGE DuplicateRecordFields #-}
{-# LANGUAGE ExplicitNamespaces #-}
{-# LANGUAGE LambdaCase #-}
{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE RecordWildCards #-}
{-# LANGUAGE ScopedTypeVariables #-}
{-# LANGUAGE TypeApplications #-}

module Main where

import Control.Lens ((^.))
import Control.Monad.IO.Class (liftIO)
import Data.Array ((!))
import Data.Text (Text)
import qualified Data.Text as T
import Language.LSP.Protocol.Lens
  ( HasContentChanges (contentChanges),
    HasParams (params),
    HasText (text),
    HasTextDocument (textDocument),
    HasUri (uri),
  )
import Language.LSP.Protocol.Message
  ( Method
      ( Method_TextDocumentDidChange,
        Method_TextDocumentDidOpen,
        Method_WorkspaceDidChangeConfiguration
      ),
    SMethod
      ( SMethod_TextDocumentDidChange,
        SMethod_TextDocumentDidOpen,
        SMethod_TextDocumentPublishDiagnostics,
        SMethod_WindowLogMessage,
        SMethod_WorkspaceDidChangeConfiguration
      ),
  )
import Language.LSP.Protocol.Types
  ( Diagnostic (..),
    DiagnosticSeverity (DiagnosticSeverity_Error),
    LogMessageParams (LogMessageParams),
    MessageType (MessageType_Info),
    Position (Position),
    PublishDiagnosticsParams (PublishDiagnosticsParams),
    Range (Range),
    TextDocumentContentChangeEvent (..),
    TextDocumentContentChangeWholeDocument (..),
    Uri,
    type (|?) (InL, InR),
  )
import Language.LSP.Server
  ( Handler,
    Handlers,
    LspM,
    ServerDefinition (..),
    defaultOptions,
    notificationHandler,
    runLspT,
    runServer,
    sendNotification,
    type (<~>) (Iso),
  )
import Text.Regex.TDFA
  ( MatchText,
    Regex,
    RegexContext (match),
    RegexMaker (makeRegex),
  )

-- | Main entry point: run the LSP server
main :: IO Int
main =
  runServer $
    ServerDefinition
      { parseConfig = const $ const $ Right (),
        onConfigChange = const $ pure (),
        defaultConfig = (),
        configSection = "markdown-lsp",
        doInitialize = \env _ -> return (Right env),
        staticHandlers = \_caps -> handlers,
        interpretHandler = \env -> Iso (runLspT env) liftIO,
        options = defaultOptions
      }

-- | Combine all handlers
handlers :: Handlers (LspM ())
handlers =
  mconcat
    [ notificationHandler SMethod_TextDocumentDidOpen didOpenHandler,
      notificationHandler SMethod_TextDocumentDidChange didChangeHandler,
      notificationHandler SMethod_WorkspaceDidChangeConfiguration workspaceDidChangeConfigurationHandler
    ]

-- -- | Handle 'initialized' notification
-- initializedHandler :: Handler (LspM ()) Method_Initialized
-- initializedHandler = \_not -> do
--   let parameters =
--         ShowMessageRequestParams
--           MessageType_Info
--           "Turn on code lenses?"
--           (Just [MessageActionItem "Turn on", MessageActionItem "Don't"])
--   _ <- sendRequest SMethod_WindowShowMessageRequest parameters $ \case
--     Right (InL (MessageActionItem "Turn on")) -> do
--       let regOpts = CodeLensRegistrationOptions (InR Null) Nothing (Just False)

--       _ <- registerCapability mempty SMethod_TextDocumentCodeLens regOpts $ \_req responder -> do
--         let cmd = Command "Say hello" "lsp-hello-command" Nothing
--             rsp = [CodeLens (mkRange 0 0 0 100) (Just cmd) Nothing]
--         responder $ Right $ InL rsp
--       pure ()
--     Right _ ->
--       sendNotification SMethod_WindowShowMessage (ShowMessageParams MessageType_Info "Not turning on code lenses")
--     Left err ->
--       sendNotification SMethod_WindowShowMessage (ShowMessageParams MessageType_Error $ "Something went wrong!\n" <> T.pack (show err))
--   pure ()

-- | Handle 'workspace/didChangeConfiguration' notification
workspaceDidChangeConfigurationHandler :: Handler (LspM ()) Method_WorkspaceDidChangeConfiguration
workspaceDidChangeConfigurationHandler = \_ -> return ()

-- | Handle 'didOpen' notification
didOpenHandler :: Handler (LspM ()) Method_TextDocumentDidOpen
didOpenHandler msg = do
  sendNotification SMethod_WindowLogMessage (LogMessageParams MessageType_Info "Received didOpen")
  let doc = msg ^. params . textDocument
      docUri = doc ^. uri
      content = doc ^. text
  analyzeDocument docUri content

-- | Handle 'didChange' notification (full sync)
didChangeHandler :: Handler (LspM ()) Method_TextDocumentDidChange
didChangeHandler msg = do
  let doc = msg ^. params . textDocument
      docUri = doc ^. uri
      changes = msg ^. params . contentChanges
      content = case changes of
        [TextDocumentContentChangeEvent t] ->
          case t of
            InR (TextDocumentContentChangeWholeDocument change) -> change
            InL (_) -> Prelude.error "Expected a single full document change"
        _ -> Prelude.error "Expected a single full document change"
  analyzeDocument docUri content

-- | Analyze document content and send diagnostics
analyzeDocument :: Uri -> Text -> LspM () ()
analyzeDocument docUri content = do
  let diagnostics = computeDiagnostics content
  sendNotification SMethod_TextDocumentPublishDiagnostics (PublishDiagnosticsParams docUri Nothing diagnostics)

-- | Compute diagnostics for the pattern [>src:PAYLOAD]
computeDiagnostics :: Text -> [Diagnostic]
computeDiagnostics content =
  let lines' = T.lines content
      pattern = "\\[>src:(.*?)\\]" :: Text -- Regex: [>src: followed by payload until ]
      go :: Integer -> Text -> [Diagnostic]
      go lineNum line =
        let regex = makeRegex pattern :: Regex
            matches = match regex line :: [MatchText Text]
            diagnostics = flip map matches $ \m ->
              let payloadMatch = m ! 1 -- Captured group 1 is PAYLOAD
                  (start, len) = snd payloadMatch -- Start offset and length
                  rangeStart = Position (fromIntegral lineNum) (fromIntegral start)
                  rangeEnd = Position (fromIntegral lineNum) (fromIntegral (start + len))
                  r = Range rangeStart rangeEnd
                  severity = Just DiagnosticSeverity_Error
                  code = Nothing
                  codeDescription = Nothing
                  source = Just "markdown-lsp"
                  message = "Invalid PAYLOAD"
               in Diagnostic
                    { _range = r,
                      _severity = severity,
                      _code = code,
                      _codeDescription = codeDescription,
                      _source = source,
                      _message = message,
                      _relatedInformation = Nothing,
                      _tags = Nothing,
                      _data_ = Nothing
                    }
         in diagnostics
   in concat $ zipWith go [0 ..] lines'
