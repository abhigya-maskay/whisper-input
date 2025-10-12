# whisper-input

[Documentation](https://github.com/abhigya-maskay/whisper-input-docs)

## CLI Usage

`whisper-input` is a wrapper around OpenAI Whisper for voice input into any application.

### Help and Version

```bash
# Display help information
whisper-input --help

# Show version
whisper-input --version
```

### Subcommands

- `daemon` - Run the whisper-input daemon
- `start-recording` - Start recording audio
- `stop-recording` - Stop recording and transcribe
- `status` - Check daemon status

### Example

```bash
# Start the daemon
whisper-input daemon

# Start recording
whisper-input start-recording

# Stop and transcribe
whisper-input stop-recording

# Check daemon status
whisper-input status
```

## Development Setup

This project uses Nix for reproducible development environments and targets GHC 9.12.

### Enter the development environment
```bash
nix develop
```

### Build the project
```bash
cabal build
```
