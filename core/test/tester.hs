import Control.Concurrent
import System.IO

main = do
  -- Make sure working dir is where the executable is!
  putStrLn "Starting tester!"

  hin <- openFile "example.hp" ReadMode
  hout <- openFile "tester.hp" WriteMode

  let emit = do
        stop <- hIsEOF hin
        if not stop then do
            line <- hGetLine hin
            hPutStrLn hout line
            hFlush hout
            threadDelay 1000
            emit
          else return ()

  emit

  hClose hin
  hClose hout

  return ()
