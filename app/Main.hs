module Main where

import MarkdownRealCode
import System.Environment (getArgs)
import System.FilePath (takeDirectory)

main :: IO ()
main = do
  args <- getArgs
  case args of
    [inputFile] -> do
      let baseDir = takeDirectory inputFile
      contents <- readFile inputFile
      compiled <- compileSuperMarkdown baseDir contents
      putStrLn compiled
    _ -> putStrLn "Usage: markdownrealcode <input file>"