{-# OPTIONS_GHC -fno-warn-missing-signatures #-}

import Control.Concurrent
--import Control.Concurrent.Chan
--import Control.Concurrent.MVar
import Control.Monad
import Control.Monad.Fix
import Control.Exception (SomeException, catch)
import Prelude hiding (catch)
import qualified Data.IntMap as IM
import Data.IORef
import Network
import Profiling.Heap.Read
import Profiling.Heap.Process
import Profiling.Heap.Network
import Profiling.Heap.Types
import System.IO

import HandleArgs

-- Start up a process to profile and a server to broadcast the stream
-- to multiple clients.
main = withSocketsDo $ do
  (portNum,exec,dir,params) <- relayArgs

  let procData = processToProfile exec dir params []
      port = PortNumber $ fromIntegral portNum

  profChan <- newChan
  stopServer <- newEmptyMVar
  names <- newIORef IM.empty

  cbres <- profileCallback (Local procData) $ \p -> do
     -- Broadcasting...
     writeChan profChan p
     -- Cleaning up the master channel that's not read by
     -- anyone.
     _ <- readChan profChan

     case p of
       SinkId ccid ccname -> modifyIORef names (IM.insert ccid ccname)
       SinkStop           -> putMVar stopServer ()
       _                  -> return ()

  case cbres of
    Nothing -> putStrLn "Error starting profile reader thread. Did you enable heap profiling?"
    Just _ -> runServer port (takeMVar stopServer) $ \chdl -> do
      -- A rather lazy way of avoiding the need to maintain an explicit
      -- client list and perform additional synchronisation...
      ownChan <- dupChan profChan

      -- Start by sending the currently known name mapping.
      ccmap <- readIORef names
      mapM_ (sendMsg chdl . putStream . uncurry SinkId) (IM.toList ccmap)

      -- Forward stream to the client.
      fix $ \sendLoop -> do
        prof <- readChan ownChan
        ok <- flip catch (const (return False) :: SomeException -> IO Bool) $ do
          sendMsg chdl . putStream $ prof
          return (prof /= SinkStop)

        when ok sendLoop

  return ()

-- Start a loop accepting connections and running arbitrary code on
-- them in separate threads, and wait for a stopping action.
runServer port waitForStop act = do
  sock <- listenOn port

  tid <- forkIO $ forever $ do
    (hdl,_host,_cport) <- accept sock
    hSetBuffering hdl LineBuffering
    forkIO (act hdl)

  -- There might be some ugly race conditions here, where clients might
  -- be left without a message before leaving the runServer subroutine.
  _ <- waitForStop
  killThread tid
