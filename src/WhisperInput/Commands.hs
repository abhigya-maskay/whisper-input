module WhisperInput.Commands
  ( runCommand,
  )
where

import Control.Concurrent.MVar (newEmptyMVar, putMVar)
import Control.Exception (SomeException, bracket_, catch)
import Data.Text (Text)
import qualified Data.Text as T
import qualified Data.Text.IO as TIO
import System.Exit
import System.IO (BufferMode (..), hPutStrLn, hSetBuffering, stderr, stdout)
import System.Posix.Process (getProcessID)
import System.Posix.Signals (Handler (..), installHandler, sigINT, sigTERM)
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
import qualified WhisperInput.Protocol as Protocol
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
  hSetBuffering stdout LineBuffering

  socketPath <- getSocketPath
  let pidPath = getPidFilePath socketPath

  putStrLn $ "Daemon: Starting on socket " ++ socketPath

  lockStatus <- checkSocketLock socketPath
  case lockStatus of
    ActiveLock pid -> do
      hPutStrLn stderr $ "Daemon already running (PID: " ++ show pid ++ ")"
      exitWith (ExitFailure 1)
    StaleLock _ -> do
      cleanupStaleLock socketPath
    NoLock -> return ()

  shutdownVar <- newEmptyMVar
  let shutdownHandler =
        Catch
          ( do
              putStrLn "Daemon: Shutdown signal received"
              putMVar shutdownVar ()
          )
  _ <- installHandler sigINT shutdownHandler Nothing
  _ <- installHandler sigTERM shutdownHandler Nothing

  pid <- getProcessID
  putStrLn $ "Daemon: Ready (PID: " ++ show pid ++ ")"

  bracket_
    (writePidFile pidPath)
    ( do
        putStrLn "Daemon: Stopped, cleaning up..."
        cleanupOnExit socketPath
    )
    ( runServer socketPath shutdownVar
        `catch` \(exc :: SomeException) -> do
          hPutStrLn stderr $ "Daemon: Server error: " ++ show exc
          exitFailure
    )

toProtocolCommand :: Command -> Maybe Protocol.Command
toProtocolCommand StartRecording = Just Protocol.Start
toProtocolCommand StopRecording = Just Protocol.Stop
toProtocolCommand Status = Just Protocol.Status
toProtocolCommand Daemon = Nothing

runCommand :: Command -> IO ()
runCommand Daemon = startDaemon
runCommand cmd = case toProtocolCommand cmd of
  Nothing -> return ()
  Just protoCmd -> do
    let wireText = Protocol.serializeCommand protoCmd
    result <- sendCommand wireText
    case result of
      Left err -> handleIPCError err
      Right response -> case protoCmd of
        Protocol.Status -> handleStatusResponse response
        _ -> handleAckResponse wireText response
