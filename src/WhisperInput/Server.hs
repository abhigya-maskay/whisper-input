module WhisperInput.Server
  ( runServer,
  )
where

import Control.Exception (bracket, catch, throw)
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
      daemonResponse <- case parseCommand commandText of
        Left errMsg -> return (Error errMsg)
        Right cmd -> processDaemonCommand daemonHandle cmd
      sendLine clientSock (formatResponse daemonResponse)
  )
    `catch` \(_ :: IOError) -> return ()

acceptLoop :: Socket -> DaemonHandle -> IO ()
acceptLoop serverSock daemonHandle = do
  (clientSock, _clientAddr) <- Socket.accept serverSock
  handleConnection daemonHandle clientSock
    `catch` \(_ :: IOError) -> return ()
  Socket.close clientSock
  acceptLoop serverSock daemonHandle

bindSocket :: FilePath -> IO Socket
bindSocket socketPath = do
  sock <- Socket.socket AF_UNIX Stream 0
  let addr = SockAddrUnix socketPath
  Socket.bind sock addr
  Socket.listen sock 5
  return sock

runServer :: FilePath -> IO ()
runServer socketPath = do
  daemonHandle <- newDaemon
  bracket
    (bindSocket socketPath)
    Socket.close
    (`acceptLoop` daemonHandle)
