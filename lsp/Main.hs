module Main where

import LSPServer (handlers)
import Language.LSP.Server
import System.Environment (getArgs)

main :: IO ()
main = do
  args <- getArgs
  runServer $
    ServerDefinition
      { defaultConfig = (),
        onConfigurationChange = \_ _ -> return $ Right (),
        doInitialize = \env _req -> return $ Right env,
        staticHandlers = handlers,
        interpretHandler = \env -> Iso (runLspT env) liftIO,
        options = defaultOptions
      }