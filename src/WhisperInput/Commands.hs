module WhisperInput.Commands
  ( runCommand,
  )
where

import Data.Text (Text)
import qualified Data.Text as T
import qualified Data.Text.IO as TIO
import System.Exit
import System.IO (stderr)
import WhisperInput.CLI (Command (..))
import WhisperInput.DaemonRunner (startDaemon)
import WhisperInput.IPC (IPCError (..), sendCommand)
import qualified WhisperInput.Protocol as Protocol

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
