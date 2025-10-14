module WhisperInput.IPC
  ( IPCError (..),
    sendCommand,
    defaultTimeout,
  )
where

import Control.Exception (Exception, IOException, bracket, try)
import Data.Text (Text)
import Network.Socket (Family (..), SockAddr (..), Socket, SocketType (..))
import qualified Network.Socket as Socket
import System.IO.Error (isDoesNotExistError, isPermissionError)
import System.Timeout (timeout)
import WhisperInput.SocketPath (getSocketPath)
import qualified WhisperInput.Transport as Transport

data IPCError
  = DaemonNotRunning
  | PermissionDenied
  | ConnectionTimeout
  | ReadTimeout
  | WriteError Text
  | MalformedResponse Text
  | UnexpectedEOF
  | OtherIOError IOException
  deriving (Show, Eq)

instance Exception IPCError

transportErrorToIPCError :: Transport.TransportError -> IPCError
transportErrorToIPCError Transport.ConnectionClosed = UnexpectedEOF
transportErrorToIPCError Transport.InvalidUTF8 = MalformedResponse "Invalid UTF-8 in response"
transportErrorToIPCError (Transport.TransportIOError ioErr) = OtherIOError ioErr

connectTimeout :: Int
connectTimeout = 2000000

readTimeout :: Int
readTimeout = 5000000

defaultTimeout :: Int
defaultTimeout = 5000000

connectWithTimeout :: FilePath -> IO (Either IPCError Socket)
connectWithTimeout socketPath = do
  result <- try $ do
    sock <- Socket.socket AF_UNIX Stream 0
    let addr = SockAddrUnix socketPath
    maybeConn <- timeout connectTimeout (Socket.connect sock addr)
    case maybeConn of
      Nothing -> do
        Socket.close sock
        return $ Left ConnectionTimeout
      Just () -> return $ Right sock
  case result of
    Left ioErr
      | isDoesNotExistError ioErr -> return $ Left DaemonNotRunning
      | isPermissionError ioErr -> return $ Left PermissionDenied
      | otherwise -> return $ Left (OtherIOError ioErr)
    Right res -> return res

sendLine :: Socket -> Text -> IO (Either IPCError ())
sendLine sock cmd = do
  result <- Transport.sendLine sock cmd
  case result of
    Left err -> return $ Left (transportErrorToIPCError err)
    Right () -> return $ Right ()

recvLine :: Socket -> IO (Either IPCError Text)
recvLine sock = do
  result <- timeout readTimeout (Transport.recvLine sock)
  case result of
    Nothing -> return $ Left ReadTimeout
    Just eitherLine -> case eitherLine of
      Left err -> return $ Left (transportErrorToIPCError err)
      Right text -> return $ Right text

sendCommand :: Text -> IO (Either IPCError Text)
sendCommand cmd = do
  socketPath <- getSocketPath
  connectWithTimeout socketPath >>= \case
    Left err -> return $ Left err
    Right sock ->
      bracket
        (return sock)
        Socket.close
        ( \s -> do
            sendLine s cmd >>= \case
              Left err -> return $ Left err
              Right () -> recvLine s
        )
