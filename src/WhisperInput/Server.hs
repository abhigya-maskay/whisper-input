module WhisperInput.Server
  ( runServer,
  )
where

import Control.Concurrent.Async (race)
import Control.Concurrent.MVar (MVar, takeMVar)
import Control.Exception (SomeException, bracket, catch, throw, try)
import Data.Text (Text)
import qualified Data.Text as T
import Network.Socket (Family (..), SockAddr (..), Socket, SocketType (..))
import qualified Network.Socket as Socket
import WhisperInput.Daemon
  ( DaemonHandle,
    newDaemon,
    processDaemonCommand,
  )
import qualified WhisperInput.Protocol as Protocol
import qualified WhisperInput.Transport as Transport

recvLine :: Socket -> IO Text
recvLine sock = do
  result <- Transport.recvLine sock
  case result of
    Left Transport.ConnectionClosed -> throw (userError "Client disconnected")
    Left Transport.InvalidUTF8 -> throw (userError "Invalid UTF-8")
    Left (Transport.TransportIOError ioErr) -> throw ioErr
    Right text -> return text

sendLine :: Socket -> Text -> IO ()
sendLine sock message = do
  result <- Transport.sendLine sock message
  case result of
    Left (Transport.TransportIOError ioErr) -> throw ioErr
    Left Transport.ConnectionClosed -> throw (userError "Connection closed during send")
    Left Transport.InvalidUTF8 -> throw (userError "Invalid UTF-8 in send")
    Right () -> return ()

handleConnection :: DaemonHandle -> Socket -> IO ()
handleConnection daemonHandle clientSock =
  ( do
      commandText <- recvLine clientSock
      putStrLn $ "Command: Received " ++ T.unpack commandText
      daemonResponse <- case Protocol.parseCommand commandText of
        Left errMsg -> return (Protocol.Error errMsg)
        Right cmd -> processDaemonCommand daemonHandle cmd
      let responseText = Protocol.serializeResponse daemonResponse
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
