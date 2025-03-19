module Main where

import MarkdownRealCode
import System.Environment (getArgs)

main :: IO ()
main = do
  args <- getArgs
  case args of
    [inputFile] -> do
      contents <- readFile inputFile
      compiled <- compileSuperMarkdown contents
      putStrLn compiled
    _ -> putStrLn "Usage: markdownrealcode <input file>"