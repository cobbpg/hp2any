{-# OPTIONS_GHC -fno-warn-name-shadowing -fno-warn-missing-signatures #-}

module HandleArgs (graphArgs, relayArgs) where

import Control.Applicative
import Control.Monad
import Data.Maybe
import System.Console.ParseArgs
import System.Directory
import System.Exit

data ArgType = Exec
             | Cwd
             | Port
               deriving (Eq, Ord, Show)

graphArgs = do
  let fixName n = if head n `notElem` "/." then "./"++n else n

  args <- parseArgsIO ArgsTrailing
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
  
  let exec = fixName <$> getArgString args Exec
      port = getArgString args Port
      params = unwords $ argsRest args

  when (exec == Nothing && port == Nothing) $ do
    putStrLn $ argsUsage args
    putStrLn $ unlines
                 ["You need to supply either a server to connect to or an executable"
                 ,"with optional working directory and parameters."
                 ]
    exitFailure

  dir <- case getArgString args Cwd of
           Just p -> Just <$> canonicalizePath p
           Nothing -> return Nothing

  let retval = case port of
                 Just p -> Left p
                 Nothing -> Right (fromJust exec,dir,params)

  return retval

relayArgs = do
  let fixName n = if head n `notElem` "/." then "./"++n else n

  args <- parseArgsIO ArgsTrailing
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
  
  let Just exec = fixName <$> getArgString args Exec
      Just port = getArgString args Port
      params = unwords $ argsRest args

  dir <- case getArgString args Cwd of
           Just p -> Just <$> canonicalizePath p
           Nothing -> return Nothing

  return (read port :: Int,exec,dir,params)
