module Main (main) where

import WhisperInput.CLI (parseCommand)
import WhisperInput.Commands (runCommand)

-- | Main entry point: parse arguments and run command.
main :: IO ()
main = parseCommand >>= runCommand
