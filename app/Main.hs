module Main where

import Control.Concurrent.Async (mapConcurrently_)
import Data.Aeson (eitherDecodeFileStrict)
import Data.List (isSuffixOf)
import MarkdownRealCode (Config (..), compileSuperMarkdown)
import System.FilePath (takeDirectory, takeFileName, (</>))
import System.FilePath.Find (always, extension, find, (==?))

-- Replace a suffix in a string
replaceSuffix :: String -> String -> String -> String
replaceSuffix suffix newSuffix str =
  if suffix `isSuffixOf` str
    then take (length str - length suffix) str ++ newSuffix
    else str

-- Compute the output file path by replacing .src.md with .gen.md
outputPath :: FilePath -> FilePath
outputPath path =
  let dir = takeDirectory path
      file = takeFileName path
      newFile = replaceSuffix ".src.md" ".gen.md" file
   in dir </> newFile

-- Process a single .src.md file
processFile :: Config -> FilePath -> IO ()
processFile config filePath = do
  let baseDir = takeDirectory filePath
  contents <- readFile filePath
  compiled <- compileSuperMarkdown config baseDir contents
  let outputFile = outputPath filePath
  writeFile outputFile compiled

main :: IO ()
main = do
  -- Read mdrc.json from the current working directory
  eConfig <- eitherDecodeFileStrict "mdrc.json"
  case eConfig of
    Left err -> putStrLn $ "Error parsing mdrc.json: " ++ err
    Right config -> do
      let rootsList = roots config
      -- Recursively find all .src.md files in the roots
      files <- concat <$> mapM (find always (extension ==? ".src.md")) rootsList
      -- Print all discovered file paths to stdout
      mapM_ putStrLn files
      -- Process all files concurrently
      mapConcurrently_ (processFile config) files