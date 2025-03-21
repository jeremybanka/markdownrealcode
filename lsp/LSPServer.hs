{-# LANGUAGE DuplicateRecordFields #-}
{-# LANGUAGE NamedFieldPuns #-}
{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE ScopedTypeVariables #-}

module LSPServer (handlers) where

import Control.Monad.IO.Class (liftIO)
import Data.Text (Text, pack, unpack)
import qualified Data.Text as T
import Language.LSP.Protocol.Message
import Language.LSP.Protocol.Types
import qualified Language.LSP.Protocol.Types as LSP
import Language.LSP.Server
import MarkdownRealCode (extractSourcePath)
import System.Directory (doesFileExist, listDirectory)
import System.FilePath (normalise, takeDirectory, takeFileName, (</>))

-- Handlers for LSP server
handlers :: Handlers (LspM ())
handlers =
  mconcat
    [ notificationHandler STextDocumentDidChange $ \NotificationMessage {_params = DidChangeTextDocumentParams {_contentChanges}} -> do
        doc <- getDocument
        validateDocument doc,
      notificationHandler STextDocumentDidOpen $ \NotificationMessage {_params = DidOpenTextDocumentParams {_textDocument}} -> do
        validateDocument $ _textDocument,
      requestHandler STextDocumentCompletion $ \RequestMessage {_params = CompletionParams {_textDocument, _position}} -> do
        doc <- getDocumentFromUri $ TextDocumentIdentifier :: _uri _textDocument
        let line = getLineAtPosition doc _position
        case extractSourcePath (unpack line) of
          Nothing -> return $ Right $ List []
          Just partialPath -> do
            let baseDir = takeDirectory $ (LSPServer :: uriToFilePath) $ TextDocumentIdentifier :: _uri _textDocument
            suggestions <- liftIO $ getFileSuggestions baseDir partialPath
            return $ Right $ List $ map (toCompletionItem . pack) suggestions
    ]

-- Validate .src.md document
validateDocument :: TextDocumentItem -> LspM () ()
validateDocument doc = do
  let uri = TextDocumentIdentifier :: _uri doc
  let text = TextDocumentIdentifier :: _text doc
  let baseDir = takeDirectory $ (LSPServer :: uriToFilePath) uri
  let lines' = T.lines text
  diagnostics <- liftIO $ mapM (validateLine baseDir) (zip [0 ..] lines')
  sendDiagnostics uri $ concat diagnostics

validateLine :: FilePath -> (Int, Text) -> IO [Diagnostic]
validateLine baseDir (lineNum, line) = do
  case extractSourcePath (unpack line) of
    Nothing -> return []
    Just srcPath -> do
      exists <- doesFileExist (baseDir </> srcPath)
      return $
        if exists
          then []
          else
            [ Diagnostic
                { _range =
                    Range
                      (Position lineNum (T.length line - T.length (pack srcPath) - 6))
                      (Position lineNum (T.length line)),
                  _severity = Just DsError,
                  _source = Just "markdownrealcode",
                  _message = "File '" <> pack srcPath <> "' does not exist.",
                  _tags = Nothing,
                  _relatedInformation = Nothing,
                  _code = Nothing,
                  _data_ = Nothing,
                  _codeDescription = Nothing
                }
            ]
  where
    getSrcStart = T.length line - T.length (T.dropWhileEnd (/= '>') line) - 5

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
getLineAtPosition doc pos = T.lines (_text doc) !! fromIntegral (_line pos)

-- Convert file path to CompletionItem
toCompletionItem :: Text -> CompletionItem
toCompletionItem label =
  CompletionItem
    { _label = label,
      _kind = Just CiFile,
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
      _tags = Nothing,
      _data_ = Nothing,
      _labelDetails = Nothing,
      _insertTextMode = Nothing,
      _textEditText = Nothing
    }

uriToFilePath :: Uri -> FilePath
uriToFilePath (Uri u) = normalise $ T.unpack $ T.drop 7 u -- Drop "file://"