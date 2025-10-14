module WhisperInput.Server
  ( runServer,
  )
where

import Control.Concurrent.Async (race)
import Control.Concurrent.MVar (MVar, takeMVar)
import Control.Exception (SomeException, bracket, catch, throw, try)
import qualified Data.ByteString as BS
import Data.Text (Text)
import qualified Data.Text as T
import qualified Data.Text.Encoding as TE
import Network.Socket (Family (..), SockAddr (..), Socket, SocketType (..))
import qualified Network.Socket as Socket
import qualified Network.Socket.ByteString as SBS
import WhisperInput.Daemon
  ( DaemonCommand (..),
    DaemonHandle,
    DaemonResponse (..),
    DaemonState (..),
    newDaemon,
    processDaemonCommand,
  )

parseCommand :: Text -> Either Text DaemonCommand
parseCommand cmd = case T.strip cmd of
  "START" -> Right Start
  "STOP" -> Right Stop
  "STATUS" -> Right Status
  _ -> Left "Unknown command"

formatResponse :: DaemonResponse -> Text
formatResponse Ack = "ACK"
formatResponse (StateReport Idle) = "STATE: Idle"
formatResponse (StateReport Recording) = "STATE: Recording"
formatResponse (Error msg) = "ERROR: " <> msg

recvLine :: Socket -> IO Text
recvLine sock = recvUntilNewline sock BS.empty
  where
    recvUntilNewline :: Socket -> BS.ByteString -> IO Text
    recvUntilNewline s accumulated = do
      chunk <- SBS.recv s 4096
      if BS.null chunk
        then throw (userError "Client disconnected")
        else do
          let combined = accumulated <> chunk
          case BS.elemIndex 10 combined of
            Nothing -> recvUntilNewline s combined
            Just nlPos -> do
              let line = BS.take nlPos combined
              case TE.decodeUtf8' line of
                Left _ -> throw (userError "Invalid UTF-8")
                Right text -> return text

sendLine :: Socket -> Text -> IO ()
sendLine sock response = do
  let responseWithNewline = if T.isSuffixOf "\n" response then response else response <> "\n"
  let bytes = TE.encodeUtf8 responseWithNewline
  SBS.sendAll sock bytes

handleConnection :: DaemonHandle -> Socket -> IO ()
handleConnection daemonHandle clientSock =
  ( do
      commandText <- recvLine clientSock
      putStrLn $ "Command: Received " ++ T.unpack commandText
      daemonResponse <- case parseCommand commandText of
        Left errMsg -> return (Error errMsg)
        Right cmd -> processDaemonCommand daemonHandle cmd
      let responseText = formatResponse daemonResponse
      sendLine clientSock responseText
      putStrLn $ "Response: Sent " ++ T.unpack responseText
  )
    `catch` \(_ :: IOError) -> return ()

acceptLoop :: Socket -> DaemonHandle -> MVar () -> IO ()
acceptLoop serverSock daemonHandle shutdownVar = do
  result <- race (takeMVar shutdownVar) (Socket.accept serverSock)
  case result of
    Left () -> return ()
    Right (clientSock, _clientAddr) -> do
      putStrLn "Client: Connected"
      handleConnection daemonHandle clientSock
        `catch` \(_ :: IOError) -> return ()
      Socket.close clientSock
      putStrLn "Client: Disconnected"
      acceptLoop serverSock daemonHandle shutdownVar

bindSocket :: FilePath -> IO Socket
bindSocket socketPath = do
  result <- try $ do
    sock <- Socket.socket AF_UNIX Stream 0
    let addr = SockAddrUnix socketPath
    Socket.bind sock addr
    Socket.listen sock 5
    putStrLn $ "Server: Socket bound to " ++ socketPath
    putStrLn "Server: Listening for connections (queue: 5)"
    return sock
  case result of
    Right sock -> return sock
    Left (exc :: SomeException) -> do
      putStrLn $ "Server: Failed to bind socket: " ++ show exc
      throw exc

runServer :: FilePath -> MVar () -> IO ()
runServer socketPath shutdownVar = do
  daemonHandle <- newDaemon
  bracket
    (bindSocket socketPath)
    Socket.close
    (\sock -> acceptLoop sock daemonHandle shutdownVar)
