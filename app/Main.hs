module Main where

import Control.Concurrent (threadDelay)
import Control.Concurrent.Async (mapConcurrently_)
import Control.Monad (forever)
import Data.Aeson (eitherDecodeFileStrict)
import Data.List (isSuffixOf)
import MarkdownRealCode (Config (..), compileSuperMarkdown)
import System.Console.GetOpt (ArgDescr (..), ArgOrder (..), OptDescr (..), getOpt)
import System.Directory (getCurrentDirectory)
import System.Environment (getArgs)
import System.FSNotify (Event (..), watchDir, withManager)
import System.FilePath (takeDirectory, takeFileName, (</>))
import System.FilePath.Find (always, fileName, find, (~~?))
import Text.Printf (printf)

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
  putStrLn $ "Processing file: " ++ filePath
  let baseDir = takeDirectory filePath
  contents <- readFile filePath
  compiled <- compileSuperMarkdown config baseDir contents
  let outputFile = outputPath filePath
  putStrLn $ "Writing to: " ++ outputFile
  writeFile outputFile compiled

-- Command-line options
data Options = Options
  { optWatch :: Bool
  }
  deriving (Show)

options :: [OptDescr (Options -> Options)]
options =
  [ Option
      ['w']
      ["watch"]
      (NoArg (\opts -> opts {optWatch = True}))
      "Watch for changes and recompile .src.md files"
  ]

parseOpts :: [String] -> IO Options
parseOpts args =
  case getOpt Permute options args of
    (o, [], []) -> return $ foldl (flip id) (Options False) o
    (_, nonOpts, []) -> error $ "Unrecognized arguments: " ++ unwords nonOpts
    (_, _, errs) -> error $ concat errs ++ "Use -h for help"

-- Watch mode: monitor directories and recompile on file changes
watchMode :: Config -> [FilePath] -> IO ()
watchMode config rootFolders = withManager $ \mgr -> do
  putStrLn "Entering watch mode. Monitoring for changes..."
  mapM_ (watchRoot mgr) rootFolders
  forever $ threadDelay 1000000 -- Keep the process alive
  where
    watchRoot mgr root = do
      let predicate event = case event of
            Modified path _ _ -> ".src.md" `isSuffixOf` path
            Added path _ _ -> ".src.md" `isSuffixOf` path
            _ -> False
      let action event = case event of
            Modified path _ _ -> do
              printf "Detected change in %s, recompiling...\n" path
              processFile config path
            Added path _ _ -> do
              printf "Detected new file %s, compiling...\n" path
              processFile config path
            _ -> return ()
      _ <- watchDir mgr root predicate action
      putStrLn $ "Watching directory: " ++ root

main :: IO ()
main = do
  args <- getArgs
  opts <- parseOpts args
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
      if optWatch opts
        then watchMode config rootsList
        else do
          return ()
