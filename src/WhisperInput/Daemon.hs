module WhisperInput.Daemon
  ( DaemonHandle,
    handleCommand,
    newDaemon,
    processDaemonCommand,
  )
where

import Control.Concurrent.MVar (MVar, modifyMVar, newMVar)
import WhisperInput.Protocol (Command (..), Response (..), State (..))

handleCommand :: State -> Command -> (State, Response)
handleCommand Idle Start = (Recording, Ack)
handleCommand Idle Stop = (Idle, Error "Already stopped")
handleCommand Idle Status = (Idle, StateReport Idle)
handleCommand Recording Start = (Recording, Error "Already recording")
handleCommand Recording Stop = (Idle, Ack)
handleCommand Recording Status = (Recording, StateReport Recording)

newtype DaemonHandle = DaemonHandle (MVar State)

newDaemon :: IO DaemonHandle
newDaemon = DaemonHandle <$> newMVar Idle

processDaemonCommand :: DaemonHandle -> Command -> IO Response
processDaemonCommand (DaemonHandle stateVar) cmd =
  modifyMVar stateVar $ \currentState ->
    return (handleCommand currentState cmd)
