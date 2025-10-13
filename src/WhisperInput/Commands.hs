module WhisperInput.Commands
  ( runCommand,
  )
where

import Data.Text (Text)
import qualified Data.Text as T
import qualified Data.Text.IO as TIO
import System.Exit
import System.IO (hPutStrLn, stderr)
import WhisperInput.CLI (Command (..))
import WhisperInput.IPC (IPCError (..), sendCommand)
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

formatIPCError :: IPCError -> Text
formatIPCError DaemonNotRunning = "Error: daemon not running"
formatIPCError PermissionDenied = "Error: permission denied accessing daemon socket"
formatIPCError ConnectionTimeout = "Error: connection to daemon timed out"
formatIPCError ReadTimeout = "Error: daemon did not respond in time"
formatIPCError (WriteError msg) = "Error: failed to send command: " <> msg
formatIPCError (MalformedResponse msg) = "Error: daemon sent invalid response: " <> msg
formatIPCError UnexpectedEOF = "Error: daemon closed connection unexpectedly"
formatIPCError (OtherIOError ioErr) = "Error: " <> T.pack (show ioErr)

handleIPCError :: IPCError -> IO ()
handleIPCError err = do
  TIO.hPutStrLn stderr (formatIPCError err)
  exitWith (ExitFailure 1)

handleStatusResponse :: Text -> IO ()
handleStatusResponse response =
  case T.stripPrefix "STATE: " response of
    Just state -> do
      TIO.putStrLn state
      exitSuccess
    Nothing
      | T.isPrefixOf "ERROR: " response -> do
          TIO.hPutStrLn stderr response
          exitWith (ExitFailure 1)
      | otherwise -> do
          TIO.hPutStrLn stderr $ "Error: unexpected response from daemon: " <> response
          exitWith (ExitFailure 1)

handleAckResponse :: Text -> Text -> IO ()
handleAckResponse commandName response
  | response == "ACK" = exitSuccess
  | T.isPrefixOf "ERROR: " response = do
      TIO.hPutStrLn stderr response
      exitWith (ExitFailure 1)
  | otherwise = do
      TIO.hPutStrLn stderr $ "Error: unexpected response to " <> commandName <> ": " <> response
      exitWith (ExitFailure 1)

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
  result <- sendCommand "START"
  case result of
    Left err -> handleIPCError err
    Right response -> handleAckResponse "START" response
runCommand StopRecording = do
  result <- sendCommand "STOP"
  case result of
    Left err -> handleIPCError err
    Right response -> handleAckResponse "STOP" response
runCommand Status = do
  result <- sendCommand "STATUS"
  case result of
    Left err -> handleIPCError err
    Right response -> handleStatusResponse response
