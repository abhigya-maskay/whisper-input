module WhisperInput.Commands
  ( runCommand,
  )
where

import System.Exit
import WhisperInput.CLI (Command (..))

-- | Execute the given command (stub implementations).
runCommand :: Command -> IO ()
runCommand Daemon = do
  putStrLn "daemon: not implemented"
  exitSuccess
runCommand StartRecording = do
  putStrLn "start: not implemented"
  exitSuccess
runCommand StopRecording = do
  putStrLn "stop: not implemented"
  exitSuccess
runCommand Status = do
  putStrLn "daemon not running"
  exitWith (ExitFailure 1)
