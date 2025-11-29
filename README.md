# dictation-app

Hyprland dictation helper that records audio while a hardware button is held, transcribes with Faster Whisper, and injects the resulting text into the focused window. Supports Wayland compositors (`wtype`/`ydotool`) and X11 sessions via `xdotool`.

## Quick Start

1. **Install dependencies** (either option works):
   - Python: `pip install -e .`
   - Nix: `nix develop` to enter the dev shell with all required tooling.
2. **Create a config file** (see [Configuration](#configuration)).
3. **List devices** to locate the correct input and audio endpoints:
   ```bash
   dictation-app list-inputs
   dictation-app list-audio
   ```
4. **Launch the daemon**:
   ```bash
   dictation-app run
   ```

## Configuration

The app loads `dictation.toml` from (in order): `--config` CLI flag, `DICTATION_CONFIG` env var, `./dictation.toml`, or `~/.config/dictation.toml`.

Minimal example:

```toml
[input]
device = "/dev/input/by-id/your-device"
key_code = "BTN_EXTRA"

[audio]
device = 2           # Optional index from `list-audio`
sample_rate = 16000  # Optional override

[model]
name = "small"
device = "cuda"

[injector]
# Backend options: "wtype"/"ydotool" for Wayland, "xdotool" for X11
backend = "wtype"
clipboard_mode = true

[orchestrator]
max_retries = 3
```

`wtype` and `ydotool` target Wayland compositors (install `wl-copy` for clipboard fallback), while `xdotool` enables X11 sessions (install `xclip` when `clipboard_mode = true`).

### Helpful Commands

- `dictation-app list-inputs [--json]` – Inspect evdev button devices; use the selected path in `[input].device`.
- `dictation-app list-audio [--json]` – Inspect capture devices; use the index in `[audio].device`.
- `dictation-app dry-run` – Validate configuration without recording or injecting text.

### Runtime Overrides

Use CLI flags to temporarily override configuration values:

```bash
dictation-app run \
  --audio-device 1 \
  --model small \
  --device cuda \
  --input-device /dev/input/by-id/new-device \
  --dry-run
```

`--dry-run` toggles the injector into logging-only mode while still exercising the full state machine.

## Workflow

1. Press and hold the configured hardware button.
2. Audio recording starts immediately and stops on release.
3. The captured WAV is transcribed with Faster Whisper.
4. Text is injected using `wtype`, `ydotool`, or `xdotool` (clipboard paste fallback when enabled).
5. Errors trigger automatic retries with exponential backoff as governed by `[orchestrator]` settings.

## Development

Install development dependencies:

```bash
pip install -e ".[dev]"
```

Or use the Nix shell for a fully reproducible toolchain:

```bash
nix develop
```

### Running tests

```bash
pytest
```

### Linting

```bash
ruff check .
black --check .
```
