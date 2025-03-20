module Main where

import Control.Concurrent.Async (mapConcurrently_)
import Data.Aeson (eitherDecodeFileStrict)
import Data.List (isSuffixOf)
import MarkdownRealCode (Config (..), compileSuperMarkdown)
import System.Directory (getCurrentDirectory)
import System.FilePath (takeDirectory, takeFileName, (</>))
import System.FilePath.Find (always, fileName, find, (~~?))

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
  putStrLn $ "Processing file: " ++ filePath -- Debug: Show the file being processed
  let baseDir = takeDirectory filePath
  putStrLn $ "Base directory: " ++ baseDir -- Debug: Show the base directory
  contents <- readFile filePath
  compiled <- compileSuperMarkdown config baseDir contents
  let outputFile = outputPath filePath
  putStrLn $ "Writing to: " ++ outputFile -- Debug: Show the output file path
  writeFile outputFile compiled

main :: IO ()
main = do
  cwd <- getCurrentDirectory
  putStrLn $ "Current working directory: " ++ cwd
  eConfig <- eitherDecodeFileStrict "mdrc.json"
  case eConfig of
    Left err -> putStrLn $ "Failed to parse mdrc.json: " ++ err
    Right config -> do
      let rootsList = roots config
      putStrLn $ "Searching in roots: " ++ show rootsList
      files <- concat <$> mapM (find always (fileName ~~? "*.src.md")) rootsList
      putStrLn "Discovered files:"
      mapM_ putStrLn files
      putStrLn "Processing files..."
      mapConcurrently_ (processFile config) files
