module WhisperInput.Commands
  ( runCommand,
  )
where

import System.Exit
import System.IO (hPutStrLn, stderr)
import WhisperInput.CLI (Command (..))
import WhisperInput.PidFile
  ( LockStatus (..),
    checkSocketLock,
    cleanupOnExit,
    cleanupStaleLock,
    getPidFilePath,
    writePidFile,
  )
import WhisperInput.Server (runServer)
import WhisperInput.SocketPath (getSocketPath)

startDaemon :: IO ()
startDaemon = do
  socketPath <- getSocketPath
  let pidPath = getPidFilePath socketPath

  lockStatus <- checkSocketLock socketPath
  case lockStatus of
    ActiveLock pid -> do
      hPutStrLn stderr $ "Daemon already running (PID: " ++ show pid ++ ")"
      exitWith (ExitFailure 1)
    StaleLock _ -> do
      cleanupStaleLock socketPath
    NoLock -> return ()

  writePidFile pidPath
  runServer socketPath
  cleanupOnExit socketPath

runCommand :: Command -> IO ()
runCommand Daemon = startDaemon
runCommand StartRecording = do
  putStrLn "start: not implemented"
  exitSuccess
runCommand StopRecording = do
  putStrLn "stop: not implemented"
  exitSuccess
runCommand Status = do
  putStrLn "daemon not running"
  exitWith (ExitFailure 1)
