module Main (main) where

import Data.Version (showVersion)
import qualified Options.Applicative as O
import qualified Paths_whisper_input
import System.Exit

-- | CLI commands for whisper-input.
data Command
  = Daemon
  | StartRecording
  | StopRecording
  | Status
  deriving (Eq, Show)

-- | Parser for CLI subcommands.
commandParser :: O.Parser Command
commandParser =
  O.hsubparser $
    O.command
      "daemon"
      (O.info (O.pure Daemon) (O.progDesc "Run the whisper-input daemon"))
      <> O.command
        "start-recording"
        (O.info (O.pure StartRecording) (O.progDesc "Start recording audio"))
      <> O.command
        "stop-recording"
        (O.info (O.pure StopRecording) (O.progDesc "Stop recording and transcribe"))
      <> O.command
        "status"
        (O.info (O.pure Status) (O.progDesc "Check daemon status"))

-- | Parser for --version flag.
versionOption :: O.Parser (a -> a)
versionOption =
  O.infoOption
    (showVersion Paths_whisper_input.version)
    (O.long "version" <> O.help "Show version")

-- | Complete parser info with subcommands, help, and version.
parserInfo :: O.ParserInfo Command
parserInfo =
  O.info (commandParser O.<**> O.helper O.<**> versionOption) $
    O.fullDesc
      <> O.progDesc "A wrapper around OpenAI Whisper for dictation"
      <> O.header "whisper-input - voice input for any application"

-- | Execute the given command (stub implementations).
runCommand :: Command -> IO ()
runCommand Daemon = do
  putStrLn "daemon: not implemented"
  exitSuccess
runCommand StartRecording = do
  putStrLn "start: not implemented"
  exitSuccess
runCommand StopRecording = do
  putStrLn "stop: not implemented"
  exitSuccess
runCommand Status = do
  putStrLn "daemon not running"
  exitWith (ExitFailure 1)

-- | Main entry point: parse arguments and run command.
main :: IO ()
main = O.execParser parserInfo >>= runCommand
