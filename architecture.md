#! Architecture: Hyprland Dictation Helper

## Overview
- Python daemon activated by holding a configurable input button; records audio while pressed, transcribes via Faster Whisper, and injects the resulting text into the currently focused surface on Hyprland Wayland or X11 sessions.
- Dependencies managed via Nix flakes for reproducible builds; Linux-only target with PipeWire audio stack and Wayland compositor support.

## Module Layout
- `main.py`: Typer CLI entrypoint accepting config path and logging flags, initializes application context and orchestrator.
- `config.py`: loads `dictation.toml`, resolves paths, validates device identifiers, exposes typed settings dataclass.
- `button_listener.py`: asyncio wrapper around `evdev.InputDevice` emitting debounced press/release events for configured key code; exposes helper command to enumerate devices.
- `recorder.py`: manages audio capture through `sounddevice` (PortAudio/PipeWire backend); start/stop APIs stream PCM into temporary buffer with optional silence trimming and format conversion.
- `transcriber.py`: encapsulates Faster Whisper `WhisperModel`, loads model lazily, runs transcription inside a thread pool executor, returns best text segment with configurable post-processing.
- `injector.py`: executes `wtype`/`ydotool` on Wayland or `xdotool` on X11 to synthesize keyboard events; clipboard fallback via `wl-copy` or `xclip` depending on the backend.
- `orchestrator.py`: central async state machine coordinating listener, recorder, transcriber, and injector; handles lifecycle events, timeouts, and error propagation, with optional notifications via `notify-send`.

## Runtime Flow
1. CLI reads configuration, prepares logging, initializes shared resources (optional preloading of Whisper model).
2. Button listener emits `Pressed` event -> orchestrator transitions to `Recording` and starts recorder.
3. Recorder streams audio until listener emits `Released`; orchestrator stops recorder, captures buffer path.
4. Transcriber processes captured audio asynchronously, applies post-processing, and returns final text.
5. Injector sends text as synthetic keystrokes (or clipboard paste) to the focused window; orchestrator resets to idle state awaiting next press.
6. Errors (audio failure, transcription timeout, injector exit code) logged and surfaced; orchestrator recovers without exiting.

## Nix Flake Structure
- Inputs: `nixpkgs`, `flake-utils`, optional `poetry2nix` if `pyproject.toml` used.
- Outputs:
  - `packages.${system}.dictation-app`: Python application wrapped with dependencies.
  - `devShells.${system}.default`: development shell with Python tooling and Wayland utilities.
- Python dependencies: `faster-whisper`, `numpy`, `sounddevice`, `evdev`, `typer`, `tomllib`/`tomli`, `onnxruntime`, `ffmpeg` for model support.
- Shell packages: `wtype`, `wl-clipboard`, `ydotool`, `xdotool`, `xclip`, `sox`, `pw-cli`, `notify-desktop` for optional notifications.

## Configuration
- `dictation.toml` keys:
  - `input.device`: `/dev/input/by-id/...` path to button device.
  - `input.key_code`: symbolic key name (e.g., `BTN_EXTRA`).
  - `audio.sample_rate`, `audio.channels`, `audio.chunk_size`.
- `model.name`, `model.device` (`cpu`, `cuda`, `auto`), `model.compute_type`.
- `injector.backend`: `wtype`, `ydotool`, or `xdotool`, plus optional clipboard mode flag.
  - `transcription.timeout`, `transcription.language`, `transcription.noise_reduction`.
- CLI helper flags: `--list-inputs`, `--list-audio`, `--dry-run` for debugging injection.

## Hyprland/Wayland Integration
- Inject keystrokes via virtual keyboard protocols; ensure user in `input` group and `wlr-virtual-keyboard` enabled.
- Optional diagnostic command using `hyprctl activewindow -j` to inspect focus before injection.
- Clipboard fallback uses `wl-copy`/`wl-paste` on Wayland or `xclip` on X11 to maintain compositor compatibility.
- X11 sessions inject keystrokes through `xdotool`; ensure `xclip` is installed if clipboard fallback is desired.

## Testing Strategy
- Unit tests with `pytest` and `pytest-asyncio` mocking evdev events, audio recorder, and transcription responses.
- Integration smoke test runs prerecorded WAV through transcriber to verify model invocation and text normalization.
- Manual validation checklist: verify device detection, hold-to-record loop, transcription accuracy, injection success, and graceful handling of microphone unavailability or model load errors.

## Assumptions and Risks
- Dedicated hardware button available as evdev device; multi-key chords out of scope.
- Faster Whisper model weights cached locally; network download handled separately.
- Focused window accepts synthetic keyboard events; some sandboxed apps may block them.
- CPU inference baseline; GPU acceleration optional via config.
