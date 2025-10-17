# Implementation Checklist: Hyprland Dictation Helper

## 1. Bootstrap Nix Flake
- [x] Initialize `flake.nix` with `nixpkgs` and `flake-utils` inputs plus basic Python app output
- [x] Define `devShell` providing Python interpreter, transcription dependencies, and Wayland tooling (`wtype`, `wl-clipboard`)
- [x] Verify shell activation and dependency imports

## 2. Scaffold Python Package Structure
- [x] Create package layout (`dictation_app/`), choose build metadata (`pyproject.toml` or `setup.cfg`)
- [x] Add `main.py` Typer CLI skeleton and configure Nix build (e.g., `poetry2nix` if applicable)

## 3. Implement Configuration Loader
- [x] Introduce `config.py` with dataclasses covering input/audio/model/injector settings
- [x] Parse `dictation.toml`, apply defaults, validate device identifiers
- [x] Expose helper commands `--list-inputs` and `--list-audio`

## 4. Build Button Listener Module
- [x] Implement asyncio-based evdev listener emitting debounced press/release events for selected key
- [x] Provide device discovery utilities
- [x] Add unit tests with synthetic evdev events

## 5. Develop Audio Recorder
- [x] Design recorder lifecycle around `sounddevice.InputStream`, capturing PCM chunks into an in-memory buffer while the stream is active
- [x] Implement `start()` to configure stream parameters from config, open the stream, and begin queuing audio frames on a background thread-safe queue
- [x] Implement `stop()` to flush queued frames, optionally trim leading/trailing silence via configurable threshold/duration, and persist audio to a temporary WAV using the stdlib `wave` module
- [x] Ensure resource cleanup (closing stream, releasing buffers) and surface errors as `RuntimeError` with descriptive messages
- [x] Expose `list_devices()` by calling `sounddevice.query_devices()`, filtering for input-capable devices, and returning `{index: name}` map with handling for backend errors
- [x] Add unit tests using `pytest` and `unittest.mock` to simulate `sounddevice` streams, silence trimming behavior, device enumeration, and file creation without touching real hardware

## 6. Integrate Faster Whisper Transcriber
- [x] Wrap `WhisperModel` with lazy initialization, configurable device/compute type, and thread-executor transcription
- [x] Add post-processing for text normalization
- [x] Mock transcription in unit tests and prepare fixture WAV for integration

## 7. Create Text Injector
- [x] Execute `wtype` (default) or `ydotool` subprocess with clipboard fallback via `wl-copy` when typing fails
- [x] Implement dry-run mode
- [x] Add robust error handling around exit codes and subprocess timeouts
- [x] Implement async execution via thread pool executor to avoid blocking event loop
- [x] Add comprehensive unit tests with subprocess mocking (30 tests, all passing)

## 8. Assemble Orchestrator
- [x] Coordinate listener, recorder, transcriber, and injector through async state machine with timeout/error management
- [x] Integrate logging and optional notifications
- [x] Implement graceful shutdown
- [x] Validate transitions with pytest-asyncio tests (22 tests for orchestrator, 160 total passing)

## 9. Wire CLI Entry Point
- [x] Connect Typer commands to orchestrator start, helper diagnostics, and config overrides
- [x] Expose runnable script via Nix flake outputs (`packages.${system}.dictation-app`)

## 10. Testing and QA
- [x] Finalize unit/integration tests and hook into `flake.nix` checks
- [x] Perform manual validation loop inside Hyprland session for hold-to-record workflow and failure modes
- [x] Document deployment and configuration usage if requested
