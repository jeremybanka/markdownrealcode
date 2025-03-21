{-# LANGUAGE BlockArguments #-}
{-# LANGUAGE DuplicateRecordFields #-}
{-# LANGUAGE LambdaCase #-}
{-# LANGUAGE NamedFieldPuns #-}
{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE RecordWildCards #-}

module LSPServer where

import Control.Lens (makeLenses, (^.))
import Control.Monad.IO.Class (liftIO)
import Control.Monad.Trans.Class (MonadTrans (lift))
import Control.Monad.Trans.Except (ExceptT, catchE)
import Control.Monad.Trans.State.Strict (StateT)
import Data.Maybe (fromMaybe)
import Data.Text (Text, pack, unpack)
import qualified Data.Text as T
import Language.LSP.Protocol.Lens
  ( HasLine (line),
    HasParams (params),
    HasPosition (position),
    HasText (text),
    HasTextDocument (textDocument),
    HasUri (uri),
  )
import Language.LSP.Protocol.Message
  ( RequestMessage (RequestMessage),
    SMethod
      ( SMethod_TextDocumentCompletion,
        SMethod_TextDocumentDidChange,
        SMethod_TextDocumentDidOpen,
        SMethod_WindowLogMessage,
        SMethod_WindowShowMessage
      ),
  )
import Language.LSP.Protocol.Types
import qualified Language.LSP.Protocol.Types as LSP
import Language.LSP.Server
import qualified Language.LSP.Server as LSP
import MarkdownRealCode (extractSourcePath)
import System.Directory (doesFileExist, listDirectory)
import System.FilePath (normalise, takeDirectory, takeFileName, (</>))

-- Handlers for LSP server
handlers :: Handlers (LspM ())
handlers =
  mconcat
    [ notificationHandler SMethod_TextDocumentDidChange $ \msg -> do
          let DidChangeTextDocumentParams {_contentChanges} = msg ^. params
          doc <- getDocument
          let uri' = doc ^. uri
              txt' = doc ^. text
              maybeFilePath = LSP.uriToFilePath uri'
              filePath = fromMaybe "" maybeFilePath
              baseDir = takeDirectory $ filePath
              lines' = T.lines txt'
          diagnostics <- liftIO $ mapM (validateLine baseDir) (zip [0 ..] lines')

        -- validateDocument doc,
        notificationHandler
        SMethod_TextDocumentDidOpen
        $ \msg -> do
          let DidOpenTextDocumentParams {_textDocument} = msg ^. params
          validateDocument _textDocument
          -- requestHandler SMethod_TextDocumentCompletion $ \request respond -> handleErrorWithDefault respond (InR (InL (CompletionList False Nothing []))) do
          --   let CompletionParams {_textDocument, _position} = request ^. params
          --   doc <- getDocumentFromUri (_textDocument ^. uri)
          --   let line = getLineAtPosition doc _position
          --   case extractSourcePath (unpack line) of
          --     Nothing -> return $ Right $ InL $ CompletionList False Nothing []
          --     Just partialPath -> do
          --       let baseDir = takeDirectory $ fromMaybe "" $ LSP.uriToFilePath (_textDocument ^. uri)
          --       suggestions <- liftIO $ getFileSuggestions baseDir partialPath
          --       return $ Right $ InL $ CompletionList False Nothing $ map (toCompletionItem . pack) suggestions

          -- \req responder -> do
          --   let CompletionParams {_textDocument, _position} = msg ^. params
          --   doc <- getDocumentFromUri (_textDocument ^. uri)
          --   let line = getLineAtPosition doc _position
          --   case extractSourcePath (unpack line) of
          --     Nothing -> return $ Right $ InL $ CompletionList False Nothing []
          --     Just partialPath -> do
          --       let baseDir = takeDirectory $ fromMaybe "" $ LSP.uriToFilePath (_textDocument ^. uri)
          --       suggestions <- liftIO $ getFileSuggestions baseDir partialPath
          --       return $ Right $ InL $ CompletionList False Nothing $ map (toCompletionItem . pack) suggestions
    ]

type HandlerM =
  ExceptT (Severity, Text) (StateT ServerState (LspT ServerConfig IO))

data Severity
  = -- | Error displayed to the user.
    Error
  | -- | Warning displayed to the user.
    Warning
  | -- | Information displayed to the user.
    Info
  | -- | Log message, not displayed by default.
    Log

data ServerState = ServerState
  {
  }

data ServerConfig = ServerConfig
  {
  }
  deriving (Show)

initialState :: ServerState
initialState = ServerState {..}
  where

-- handleErrorWithDefault respond (InR (InL (CompletionList False Nothing []))) do
completionHandler :: Handlers HandlerM
completionHandler = requestHandler SMethod_TextDocumentCompletion $ \request respond ->
  return 30

-- let CompletionParams {_textDocument, _position} = request ^. params
-- doc <- getDocumentFromUri (_textDocument ^. uri)dd
-- let line = getLineAtPosition doc _position
-- case extractSourcePath (unpack line) of
--   Nothing -> return $ Right $ InL $ CompletionList False Nothing []
--   Just partialPath -> do
--     let baseDir = takeDirectory $ fromMaybe "" $ LSP.uriToFilePath (_textDocument ^.d uri)
--     suggestions <- liftIO $ getFileSuggestions baseDir partialPath
--     return $ Right $ InL $ CompletionList False Nothing $ map (toCompletionItem . pack) suggestions

liftLSP :: LspT ServerConfig IO a -> HandlerM a
liftLSP m = lift (lift m)

handleErrorWithDefault ::
  (Either a1 b -> HandlerM a2) ->
  b ->
  HandlerM a2 ->
  HandlerM a2
handleErrorWithDefault respond _default = flip catchE handler
  where
    handler (Log, _message) = do
      let _type_ = MessageType_Log
      liftLSP $ LSP.sendNotification SMethod_WindowLogMessage LogMessageParams {..}
      respond (Right _default)
    handler (severity_, _message) = do
      let _type_ = case severity_ of
            Error -> MessageType_Error
            Warning -> MessageType_Warning
            Info -> MessageType_Info

      liftLSP $ LSP.sendNotification SMethod_WindowShowMessage ShowMessageParams {..}
      respond (Right _default)

-- Validate .src.md document
validateDocument :: TextDocumentItem -> LspM () ()
validateDocument doc = do
  let uri_ = doc ^. uri
  let txt_ = doc ^. text
  let maybeFilePath = LSP.uriToFilePath uri_
  let filePath = fromMaybe "" maybeFilePath
  let baseDir = takeDirectory $ filePath
  let lines' = T.lines txt_
  diagnostics <- liftIO $ mapM (validateLine baseDir) (zip [0 ..] lines')
  sendDiagnostics uri_ $ concat diagnostics

validateLine :: FilePath -> (Int, Text) -> IO [Diagnostic]
validateLine baseDir (lineNum, ln) = do
  case extractSourcePath (unpack ln) of
    Nothing -> return []
    Just srcPath -> do
      exists <- doesFileExist (baseDir </> srcPath)
      return $
        if exists
          then []
          else
            [ Diagnostic
                { _range = Range (Position lineNum (T.length ln - T.length (pack srcPath) - 6)) (Position lineNum (T.length ln)),
                  _severity = Just DiagnosticSeverity_Error,
                  _source = Just "markdownrealcode",
                  _message = "File '" <> pack srcPath <> "' does not exist.",
                  _tags = Nothing,
                  _relatedInformation = Nothing,
                  _code = Nothing,
                  _codeDescription = Nothing,
                  _data_ = Nothing
                }
            ]
  where
    getSrcStart = T.length ln - T.length (T.dropWhileEnd (/= '>') ln) - 5

-- Autocomplete suggestions
getFileSuggestions :: FilePath -> String -> IO [String]
getFileSuggestions baseDir partialPath = do
  let dir = baseDir </> takeDirectory partialPath
  let prefix = takeFileName partialPath
  files <- listDirectory dir
  return $ filter (isPrefixOf prefix) files
  where
    isPrefixOf p f = take (length p) f == p

-- Helper to get the document
getDocument :: LspM () TextDocumentItem
getDocument = do
  docs <- getDocs
  uri <- getCurrentDocUri
  case lookup uri docs of
    Just doc -> return doc
    Nothing -> error "Document not found"

getDocumentFromUri :: Uri -> LspM () TextDocumentItem
getDocumentFromUri uri = do
  docs <- getDocs
  case lookup uri docs of
    Just doc -> return doc
    Nothing -> error "Document not found"

getCurrentDocUri :: LspM () Uri
getCurrentDocUri = do
  m <- getLspEnv
  case documents m of
    Just docMap -> return $ fst $ head $ documentMap docMap
    Nothing -> error "No documents open"

getLineAtPosition :: TextDocumentItem -> Position -> Text
getLineAtPosition doc pos = T.lines (doc ^. text) !! fromIntegral (pos ^. line)

-- Convert file path to CompletionItem
toCompletionItem :: Text -> CompletionItem
toCompletionItem label =
  CompletionItem
    { _label = label,
      _kind = Nothing,
      _detail = Nothing,
      _documentation = Nothing,
      _deprecated = Nothing,
      _preselect = Nothing,
      _sortText = Nothing,
      _filterText = Nothing,
      _insertText = Nothing,
      _insertTextFormat = Nothing,
      _textEdit = Nothing,
      _additionalTextEdits = Nothing,
      _commitCharacters = Nothing,
      _command = Nothing,
      _labelDetails = Nothing,
      _tags = Nothing,
      _insertTextMode = Nothing,
      _textEditText = Nothing,
      _data_ = Nothing
    }

uriToFilePath :: Uri -> FilePath
uriToFilePath (Uri u) = normalise $ T.unpack $ T.drop 7 u -- Drop "file://"