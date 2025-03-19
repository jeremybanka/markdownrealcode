module MarkdownRealCode where

import Data.List (uncons)
import System.FilePath (takeExtension) -- Explicitly specify package
import Text.Printf (printf)

-- Global constant for the repository prefix
repo :: String
repo = "https://github.com/your-username/your-project/tree/main/"

-- Convert Super Markdown to Markdown
compileSuperMarkdown :: String -> IO String
compileSuperMarkdown input = do
  let lines' = lines input
  compiledLines <- mapM processLine lines'
  return $ unlines compiledLines

-- Process a single line, replacing [>src:/path] with link + code block
processLine :: String -> IO String
processLine line =
  case extractSourcePath line of
    Nothing -> return line
    Just path -> do
      contents <- readFile path -- Read the file contents
      let link = makeLink path
      let codeBlock = makeCodeBlock path contents
      return $ link ++ "\n\n" ++ codeBlock

-- Extract the path from a [>src:/path] reference
extractSourcePath :: String -> Maybe String
extractSourcePath line =
  case splitOn "[>src:" line of
    (_ : rest : _) ->
      case splitOn "]" rest of
        (path : _) -> Just path
        _ -> Nothing
    _ -> Nothing

-- Split a string on a delimiter (simple, not handling edge cases like regex)
splitOn :: String -> String -> [String]
splitOn delim str =
  case uncons delim of
    Nothing -> [str] -- If the delimiter is empty, return the entire string
    Just (firstChar, _) ->
      case break (== firstChar) str of
        (before, []) -> [before]
        (before, after) -> before : splitOn delim (drop (length delim) after)

-- Generate the markdown link: [path](repo/path)
makeLink :: String -> String
makeLink path = printf "[%s](%s%s)" path repo path

-- Generate the code block with language inferred from extension
makeCodeBlock :: String -> String -> String
makeCodeBlock path contents =
  let lang = inferLanguage path
   in printf "```%s\n%s\n```" lang contents

-- Infer the language from the file extension
inferLanguage :: String -> String
inferLanguage path =
  case takeExtension path of
    ".ts" -> "ts"
    ".hs" -> "haskell"
    ".py" -> "python"
    ".js" -> "javascript"
    ext -> drop 1 ext -- Default to extension without dot