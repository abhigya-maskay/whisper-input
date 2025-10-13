module WhisperInput.IPC
  ( IPCError (..),
    sendCommand,
    defaultTimeout,
  )
where

import Control.Exception (Exception, IOException, bracket, try)
import qualified Data.ByteString as BS
import Data.Text (Text)
import qualified Data.Text as T
import qualified Data.Text.Encoding as TE
import Network.Socket (Family (..), SockAddr (..), Socket, SocketType (..))
import qualified Network.Socket as Socket
import qualified Network.Socket.ByteString as SBS
import System.IO.Error (isDoesNotExistError, isPermissionError)
import System.Timeout (timeout)
import WhisperInput.SocketPath (getSocketPath)

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
  let cmdWithNewline = if T.isSuffixOf "\n" cmd then cmd else cmd <> "\n"
  let bytes = TE.encodeUtf8 cmdWithNewline
  result <- try $ SBS.sendAll sock bytes :: IO (Either IOException ())
  case result of
    Left ioErr -> return $ Left (WriteError (T.pack $ show ioErr))
    Right () -> return $ Right ()

recvLine :: Socket -> IO (Either IPCError Text)
recvLine sock = do
  result <- timeout readTimeout (recvUntilNewline sock BS.empty)
  case result of
    Nothing -> return $ Left ReadTimeout
    Just eitherLine -> return eitherLine

recvUntilNewline :: Socket -> BS.ByteString -> IO (Either IPCError Text)
recvUntilNewline sock accumulated = do
  result <- try $ SBS.recv sock 4096
  case result of
    Left ioErr -> return $ Left (OtherIOError ioErr)
    Right chunk
      | BS.null chunk -> return $ Left UnexpectedEOF
      | otherwise -> do
          let combined = accumulated <> chunk
          case BS.elemIndex 10 combined of
            Nothing -> recvUntilNewline sock combined
            Just nlPos -> do
              let line = BS.take nlPos combined
              case TE.decodeUtf8' line of
                Left _ -> return $ Left (MalformedResponse "Invalid UTF-8 in response")
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
