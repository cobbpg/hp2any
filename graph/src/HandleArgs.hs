{-# OPTIONS_GHC -fno-warn-name-shadowing -fno-warn-missing-signatures #-}

module HandleArgs (graphArgs, relayArgs) where

import Control.Applicative
import Control.Exception
import Control.Monad
import Data.Maybe
import System.Console.ParseArgs
import System.Directory
import System.Exit

data ArgType = Exec
             | Cwd
             | Port
               deriving (Eq, Ord, Show)

commonArgs args = do
  let canonMaybe a = case getArgString args a of
                       Just p  -> Just <$> catch (canonicalizePath p) (ioHandler p)
                       Nothing -> return Nothing
        where
          ioHandler :: a -> IOException -> IO a
          ioHandler x _ = return x

  exec <- canonMaybe Exec
  dir <- canonMaybe Cwd

  return (exec,dir,argsRest args)

graphArgs = do
  args <- parseArgsIO (ArgsTrailing "rest")
          [ Arg Exec (Just 'e') (Just "exec")
            (argDataOptional "executable" ArgtypeString)
            "Executable to profile."
          , Arg Cwd (Just 'd') (Just "cwd")
            (argDataOptional "directory" ArgtypeString)
            "Working directory of the executable."
          , Arg Port (Just 's') (Just "server")
            (argDataOptional "address:port" ArgtypeString)
            "Server to connect to."
          ]

  let port = getArgString args Port
  (exec,dir,params) <- commonArgs args

  when (exec == Nothing && port == Nothing) $ do
    putStrLn $ argsUsage args
    putStrLn $ unlines
                 [ "You need to supply either a server to connect to or an executable"
                 , "with optional working directory and parameters."
                 ]
    exitFailure

  -- Remote mode overrides local mode.
  let retval = case port of
                 Just p -> Left p
                 Nothing -> Right (fromJust exec,dir,params)

  return retval

relayArgs = do
  args <- parseArgsIO (ArgsTrailing "rest")
          [ Arg Exec (Just 'e') (Just "exec")
            (argDataRequired "executable" ArgtypeString)
            "Executable to profile."
          , Arg Cwd (Just 'd') (Just "cwd")
            (argDataOptional "directory" ArgtypeString)
            "Working directory of the executable."
          , Arg Port (Just 'p') (Just "port")
            (argDataRequired "portnum" ArgtypeString)
            "Number of server port to listen on."
          ]

  let Just port = getArgString args Port
  (exec,dir,params) <- commonArgs args

  return (read port :: Int,fromJust exec,dir,params)
