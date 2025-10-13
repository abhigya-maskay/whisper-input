module WhisperInput.SocketPath
  ( getSocketPath,
    getSocketDir,
    ensureSocketDir,
  )
where

import System.Directory (createDirectoryIfMissing)
import System.Environment (lookupEnv)
import System.FilePath ((</>))
import System.Posix.Files (setFileMode)
import System.Posix.User (getRealUserID)

socketFilename :: FilePath
socketFilename = "whisper-input.sock"

-- | Get the directory where the socket should be created.
-- Checks XDG_RUNTIME_DIR, falls back to /tmp/whisper-input-<UID>.
getSocketDir :: IO FilePath
getSocketDir = do
  maybeXdgDir <- lookupEnv "XDG_RUNTIME_DIR"
  case maybeXdgDir of
    Just xdgDir -> return xdgDir
    Nothing -> do
      uid <- getRealUserID
      return $ "/tmp/whisper-input-" ++ show uid

-- | Ensure the socket directory exists with secure permissions (0o700).
ensureSocketDir :: IO ()
ensureSocketDir = do
  dir <- getSocketDir
  createDirectoryIfMissing True dir
  setFileMode dir 0o700

-- | Get the full socket path, ensuring the directory exists.
getSocketPath :: IO FilePath
getSocketPath = do
  ensureSocketDir
  dir <- getSocketDir
  return $ dir </> socketFilename
