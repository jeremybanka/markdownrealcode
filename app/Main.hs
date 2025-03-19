module Main where

import Data.Aeson (eitherDecodeFileStrict)
import MarkdownRealCode (Config, compileSuperMarkdown)
import System.Environment (getArgs)
import System.FilePath (takeDirectory, (</>))

main :: IO ()
main = do
  args <- getArgs
  case args of
    [inputFile] -> do
      let baseDir = takeDirectory inputFile
      let configPath = baseDir </> "mdrc.json"
      eConfig <- eitherDecodeFileStrict configPath
      case eConfig of
        Left err -> putStr $ "Error parsing mdrc.json: " ++ err
        Right config -> do
          contents <- readFile inputFile
          compiled <- compileSuperMarkdown config baseDir contents
          putStr compiled
    _ -> putStr "Usage: markdownrealcode <input file>"