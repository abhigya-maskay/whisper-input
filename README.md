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
- The hook only processes `.hs` and `.lhs` files that are staged for commit

**Troubleshooting:**
- If you see "ormolu not found" or "hlint not found", make sure you're in `nix develop`
- To bypass the hook (not recommended): `git commit --no-verify`
# Test change
