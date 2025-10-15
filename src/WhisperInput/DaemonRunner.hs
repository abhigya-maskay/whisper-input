module WhisperInput.DaemonRunner
  ( startDaemon,
  )
where

import Control.Concurrent.MVar (newEmptyMVar, putMVar)
import Control.Exception (SomeException, bracket_, catch)
import System.Exit
import System.IO (BufferMode (..), hPutStrLn, hSetBuffering, stderr, stdout)
import System.Posix.Process (getProcessID)
import System.Posix.Signals (Handler (..), installHandler, sigINT, sigTERM)
import WhisperInput.PidFile
  ( LockStatus (..),
    checkSocketLock,
    cleanupOnExit,
    cleanupStaleLock,
    getPidFilePath,
    writePidFile,
  )
import WhisperInput.Server (runServer)
import WhisperInput.SocketPath (getSocketPath)

startDaemon :: IO ()
startDaemon = do
  hSetBuffering stdout LineBuffering

  socketPath <- getSocketPath
  let pidPath = getPidFilePath socketPath

  putStrLn $ "Daemon: Starting on socket " ++ socketPath

  lockStatus <- checkSocketLock socketPath
  case lockStatus of
    ActiveLock pid -> do
      hPutStrLn stderr $ "Daemon already running (PID: " ++ show pid ++ ")"
      exitWith (ExitFailure 1)
    StaleLock _ -> do
      cleanupStaleLock socketPath
    NoLock -> return ()

  shutdownVar <- newEmptyMVar
  let shutdownHandler =
        Catch
          ( do
              putStrLn "Daemon: Shutdown signal received"
              putMVar shutdownVar ()
          )
  _ <- installHandler sigINT shutdownHandler Nothing
  _ <- installHandler sigTERM shutdownHandler Nothing

  pid <- getProcessID
  putStrLn $ "Daemon: Ready (PID: " ++ show pid ++ ")"

  bracket_
    (writePidFile pidPath)
    ( do
        putStrLn "Daemon: Stopped, cleaning up..."
        cleanupOnExit socketPath
    )
    ( runServer socketPath shutdownVar
        `catch` \(exc :: SomeException) -> do
          hPutStrLn stderr $ "Daemon: Server error: " ++ show exc
          exitFailure
    )
