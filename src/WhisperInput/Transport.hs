module WhisperInput.Transport
  ( TransportError (..),
    recvLine,
    sendLine,
  )
where

import Control.Exception (IOException, try)
import qualified Data.ByteString as BS
import Data.Text (Text)
import qualified Data.Text as T
import qualified Data.Text.Encoding as TE
import Network.Socket (Socket)
import qualified Network.Socket.ByteString as SBS

data TransportError
  = ConnectionClosed
  | InvalidUTF8
  | TransportIOError IOException
  deriving (Show, Eq)

recvLine :: Socket -> IO (Either TransportError Text)
recvLine sock = recvUntilNewline sock BS.empty

recvUntilNewline :: Socket -> BS.ByteString -> IO (Either TransportError Text)
recvUntilNewline sock accumulated = do
  result <- try $ SBS.recv sock 4096
  case result of
    Left ioErr -> return $ Left (TransportIOError ioErr)
    Right chunk
      | BS.null chunk -> return $ Left ConnectionClosed
      | otherwise -> do
          let combined = accumulated <> chunk
          case BS.elemIndex 10 combined of
            Nothing -> recvUntilNewline sock combined
            Just nlPos -> do
              let line = BS.take nlPos combined
              case TE.decodeUtf8' line of
                Left _ -> return $ Left InvalidUTF8
                Right text -> return $ Right text

sendLine :: Socket -> Text -> IO (Either TransportError ())
sendLine sock message = do
  let messageWithNewline = if T.isSuffixOf "\n" message then message else message <> "\n"
  let bytes = TE.encodeUtf8 messageWithNewline
  result <- try $ SBS.sendAll sock bytes
  case result of
    Left ioErr -> return $ Left (TransportIOError ioErr)
    Right () -> return $ Right ()
