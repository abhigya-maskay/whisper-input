module WhisperInput.Daemon
  ( DaemonState (..),
    DaemonCommand (..),
    DaemonResponse (..),
    DaemonHandle,
    handleCommand,
    newDaemon,
    processDaemonCommand,
  )
where

import Control.Concurrent.MVar (MVar, modifyMVar, newMVar)
import Data.Text (Text)

data DaemonState
  = Idle
  | Recording
  deriving (Show, Eq)

data DaemonCommand
  = Start
  | Stop
  | Status
  deriving (Show, Eq)

data DaemonResponse
  = Ack
  | StateReport DaemonState
  | Error Text
  deriving (Show, Eq)

handleCommand :: DaemonState -> DaemonCommand -> (DaemonState, DaemonResponse)
handleCommand Idle Start = (Recording, Ack)
handleCommand Idle Stop = (Idle, Error "Already stopped")
handleCommand Idle Status = (Idle, StateReport Idle)
handleCommand Recording Start = (Recording, Error "Already recording")
handleCommand Recording Stop = (Idle, Ack)
handleCommand Recording Status = (Recording, StateReport Recording)

newtype DaemonHandle = DaemonHandle (MVar DaemonState)

newDaemon :: IO DaemonHandle
newDaemon = DaemonHandle <$> newMVar Idle

processDaemonCommand :: DaemonHandle -> DaemonCommand -> IO DaemonResponse
processDaemonCommand (DaemonHandle stateVar) cmd =
  modifyMVar stateVar $ \currentState ->
    return (handleCommand currentState cmd)
