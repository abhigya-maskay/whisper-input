module WhisperInput.PidFile
  ( LockStatus (..),
    getPidFilePath,
    writePidFile,
    readPidFile,
    isProcessAlive,
    removePidFile,
    checkSocketLock,
    cleanupStaleLock,
    cleanupOnExit,
  )
where

import Control.Exception (IOException, catch)
import System.Directory (doesFileExist, removeFile)
import System.FilePath (replaceExtension)
import System.Posix.Files (setFileMode)
import System.Posix.Process (getProcessID)
import System.Posix.Signals (nullSignal, signalProcess)
import System.Posix.Types (ProcessID)

data LockStatus
  = NoLock
  | StaleLock ProcessID
  | ActiveLock ProcessID
  deriving (Show, Eq)

getPidFilePath :: FilePath -> FilePath
getPidFilePath socketPath = replaceExtension socketPath ".pid"

writePidFile :: FilePath -> IO ()
writePidFile pidPath = do
  pid <- getProcessID
  writeFile pidPath (show pid)
  setFileMode pidPath 0o600

readPidFile :: FilePath -> IO (Maybe ProcessID)
readPidFile pidPath = do
  exists <- doesFileExist pidPath
  if not exists
    then return Nothing
    else
      do
        contents <- readFile pidPath
        case reads contents of
          [(pid, _)] -> return (Just pid)
          _ -> return Nothing
        `catch` handleIOError
  where
    handleIOError :: IOException -> IO (Maybe ProcessID)
    handleIOError _ = return Nothing

isProcessAlive :: ProcessID -> IO Bool
isProcessAlive pid =
  (signalProcess nullSignal pid >> return True)
    `catch` \(_ :: IOException) -> return False

removePidFile :: FilePath -> IO ()
removePidFile pidPath =
  removeFile pidPath `catch` \(_ :: IOException) -> return ()

checkSocketLock :: FilePath -> IO LockStatus
checkSocketLock socketPath = do
  socketExists <- doesFileExist socketPath
  if not socketExists
    then return NoLock
    else do
      let pidPath = getPidFilePath socketPath
      maybePid <- readPidFile pidPath
      case maybePid of
        Nothing -> return (StaleLock 0)
        Just pid -> do
          alive <- isProcessAlive pid
          return $ if alive then ActiveLock pid else StaleLock pid

cleanupStaleLock :: FilePath -> IO ()
cleanupStaleLock socketPath = do
  let pidPath = getPidFilePath socketPath
  removeFile socketPath `catch` \(_ :: IOException) -> return ()
  removePidFile pidPath

cleanupOnExit :: FilePath -> IO ()
cleanupOnExit = cleanupStaleLock
