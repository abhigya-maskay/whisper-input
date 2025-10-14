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

This will provide you with:
- GHC 9.12
- Cabal build tool
- Haskell Language Server (HLS) for editor support
- Ormolu formatter
- HLint linter

### Build the project
```bash
cabal build
```

### Code Quality and Git Hooks

This project uses a pre-commit hook to maintain code quality. The hook runs automatically when you commit changes from within `nix develop`.

**What the hook does:**
1. **Formats** all staged Haskell files with Ormolu
2. **Re-stages** the formatted files
3. **Lints** with HLint and fails the commit if issues are found

**Important notes:**
- The hook only runs when you're inside `nix develop` (it needs the tools)
- Formatting happens automatically - your code will be formatted before committing
- If HLint finds issues, the commit will be rejected. Fix the issues and try again.
- The hook only processes `.hs` files that are staged for commit

**Troubleshooting:**
- If you see "ormolu not found" or "hlint not found", make sure you're in `nix develop`
- To bypass the hook (not recommended): `git commit --no-verify`

## Daemon Lifecycle and IPC

### Daemon lifecycle
- Start: `whisper-input daemon`
  - Logs: `Daemon: Starting on socket <path>`, `Daemon: Ready (PID: <pid>)`, `Server: Socket bound to <path>`, `Server: Listening for connections`.
- Single instance: Starting a second daemon prints `Daemon already running (PID: <pid>)` and exits non‑zero.
- Stop: Send `SIGINT` or `SIGTERM` (e.g., `Ctrl-C` or `kill -TERM <pid>`)
  - Logs: `Daemon: Shutdown signal received`, then `Daemon: Stopped, cleaning up...`.
  - Cleanup: Removes the Unix socket and `.pid` file so the next start is clean.

### IPC details
- Socket path: `$XDG_RUNTIME_DIR/whisper-input.sock`
  - Fallback: `/tmp/whisper-input-<UID>/whisper-input.sock` when `XDG_RUNTIME_DIR` is unset.
  - Directory is created with `0700` permissions if missing.
- Text protocol (one line per message):
  - Commands: `START`, `STOP`, `STATUS`
  - Responses: `ACK`, `ERROR: <message>`, `STATE: Idle` | `STATE: Recording`

### CLI ↔ IPC mapping and exit codes
- `whisper-input start-recording` → sends `START`; exits `0` on `ACK`, non‑zero on `ERROR:`.
- `whisper-input stop-recording` → sends `STOP`; exits `0` on `ACK`, non‑zero on `ERROR:`.
- `whisper-input status` → sends `STATUS`; prints `Idle` or `Recording` on success and exits `0`.
- When the daemon is unreachable, commands print an error to stderr and exit non‑zero.
