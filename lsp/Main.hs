{-# LANGUAGE DataKinds #-}
{-# LANGUAGE DuplicateRecordFields #-}
{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE RecordWildCards #-}
{-# LANGUAGE ScopedTypeVariables #-}
{-# LANGUAGE TypeApplications #-}

module Main where

import Control.Lens ((^.))
import Control.Monad.IO.Class (liftIO)
import Control.Monad.Trans.Except (ExceptT)
import Control.Monad.Trans.State.Strict (StateT)
import Data.Text (Text)
import qualified Data.Text as T
import Language.LSP.Protocol.Lens as LSP
import Language.LSP.Protocol.Message
  ( Method
      ( Method_TextDocumentDidChange,
        Method_TextDocumentDidOpen
      ),
    NotificationMessage,
    SMethod (..),
  )
import Language.LSP.Protocol.Types
import Language.LSP.Server
import Text.Regex.TDFA
import Text.Regex.TDFA.Text

-- | Main entry point: run the LSP server
main :: IO ()
main = runServer serverDef

-- | Server definition
serverDef :: ServerDefinition ()
serverDef =
  ServerDefinition
    { defaultConfig = (), -- No custom configuration needed
      onConfigChange = \_ _ -> Right (), -- No config changes to handle
      doInitialize = \env _ -> return (Right env), -- Simple initialization
      staticHandlers = const handlers, -- Handlers for LSP messages
      interpretHandler = \env -> Iso (runLspT env) liftIO, -- Run in IO
      options = opts -- Server options
    }

-- | Server options: enable full text document sync
opts :: Options
opts =
  def
    { _textDocumentSync = Just (InL syncOptions)
    }
  where
    syncOptions =
      TextDocumentSyncOptions
        { _openClose = Just True, -- Handle open/close events
          _change = Just TextDocumentSyncKind_Full, -- Full sync on changes
          _willSave = Just False, -- No save handling
          _willSaveWaitUntil = Just False, -- No pre-save handling
          _save = Just (InL SaveOptions {_includeText = Just False})
        }

-- | Combine all handlers
handlers :: Handlers (LspM ())
handlers =
  mconcat
    [ notificationHandler SMethod_TextDocumentDidOpen didOpenHandler,
      notificationHandler SMethod_TextDocumentDidChange didChangeHandler
    ]

-- | Handle 'didOpen' notification
didOpenHandler :: NotificationMessage 'Method_TextDocumentDidOpen -> LspM ()
didOpenHandler msg = do
  let doc = msg ^. params . textDocument
      uri = doc ^. uri
      content = doc ^. text
  analyzeDocument uri content

-- | Handle 'didChange' notification (full sync)
didChangeHandler :: NotificationMessage 'Method_TextDocumentDidChange -> LspM ()
didChangeHandler msg = do
  let doc = msg ^. params . textDocument
      uri = doc ^. uri
      changes = msg ^. params . contentChanges
      content = case changes of
        [TextDocumentContentChangeWholeDocument newContent] -> newContent
        _ -> LSP.error "Invalid content changes"
  analyzeDocument uri content

-- | Analyze document content and send diagnostics
analyzeDocument :: Uri -> Text -> LspM ()
analyzeDocument uri content = do
  let diagnostics = computeDiagnostics content
  sendNotification SMethod_PublishDiagnostics (PublishDiagnosticsParams uri Nothing diagnostics)

-- | Compute diagnostics for the pattern [>src:PAYLOAD]
computeDiagnostics :: Text -> [Diagnostic]
computeDiagnostics content =
  let lines' = T.lines content
      pattern = "\\[>src:(.*?)\\]" :: Text -- Regex: [>src: followed by payload until ]
      go lineNum line =
        let regex = makeRegex pattern :: Regex
            matches = match regex line :: [MatchText Text]
            diagnostics = flip map matches $ \m ->
              let payloadMatch = m ! 1 -- Captured group 1 is PAYLOAD
                  (start, len) = snd payloadMatch -- Start offset and length
                  rangeStart = Position lineNum (fromIntegral start)
                  rangeEnd = Position lineNum (fromIntegral (start + len))
                  range = Range rangeStart rangeEnd
                  severity = Just DiagnosticSeverity_Error
                  code = Nothing
                  source = Just "markdown-lsp"
                  message = "Invalid PAYLOAD"
               in Diagnostic range severity code source message Nothing Nothing
         in diagnostics
   in concat $ zipWith go [0 ..] lines'