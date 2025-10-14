module WhisperInput.Protocol
  ( -- * Core types
    Command (..),
    Response (..),
    State (..),

    -- * Wire protocol
    serializeCommand,
    parseCommand,
    serializeResponse,
  )
where

import Data.Text (Text)
import qualified Data.Text as T

data State
  = Idle
  | Recording
  deriving (Show, Eq)

data Command
  = Start
  | Stop
  | Status
  deriving (Show, Eq)

data Response
  = Ack
  | StateReport State
  | Error Text
  deriving (Show, Eq)

serializeCommand :: Command -> Text
serializeCommand = \case
  Start -> "START"
  Stop -> "STOP"
  Status -> "STATUS"

parseCommand :: Text -> Either Text Command
parseCommand cmd = case T.strip cmd of
  "START" -> Right Start
  "STOP" -> Right Stop
  "STATUS" -> Right Status
  _ -> Left "Unknown command"

serializeResponse :: Response -> Text
serializeResponse = \case
  Ack -> "ACK"
  StateReport Idle -> "STATE: Idle"
  StateReport Recording -> "STATE: Recording"
  Error msg -> "ERROR: " <> msg
