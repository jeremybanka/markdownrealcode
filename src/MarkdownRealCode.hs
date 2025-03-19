module MarkdownRealCode where

-- Explicitly specify package

import Data.Function ((&))
import Data.List (isPrefixOf, uncons)
import System.FilePath (pathSeparator, takeExtension, (</>))
import Text.Printf (printf)

-- Global constant for the repository prefix
repo :: String
repo = "https://github.com/your-username/your-project/tree/main/"

-- Convert Super Markdown to Markdown
compileSuperMarkdown :: FilePath -> String -> IO String
compileSuperMarkdown baseDir input = do
  let lines' = lines input
  compiledLines <- mapM (processLine baseDir) lines'
  return $ unlines compiledLines

-- Process a single line, replacing [>src:/path] with link + code block
processLine :: FilePath -> String -> IO String
processLine baseDir line =
  case extractSourcePath line of
    Nothing -> return line
    Just relativePath -> do
      let fullPath = baseDir </> relativePath
      let urlPath = toUrlPath (removeLeadingDotSlash fullPath)
      contents <- readFile fullPath
      let link = makeLink relativePath urlPath
      let codeBlock = makeCodeBlock fullPath contents
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
makeLink :: String -> String -> String
makeLink linkText urlPath = printf "[%s](%s%s)" linkText repo urlPath

-- Generate the code block with language inferred from extension
makeCodeBlock :: FilePath -> String -> String
makeCodeBlock path contents =
  let lang = inferLanguage path
   in printf "```%s\n%s\n```" lang contents

-- Infer the language from the file extension
inferLanguage :: FilePath -> String
inferLanguage path = path & takeExtension & drop 1

removeLeadingDotSlash :: FilePath -> FilePath
removeLeadingDotSlash path =
  if "./" `isPrefixOf` path
    then drop 2 path
    else path

toUrlPath :: FilePath -> String
toUrlPath = map (\c -> if c == pathSeparator then '/' else c)