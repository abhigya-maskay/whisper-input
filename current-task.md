# Current Task: Step 3 – Implement Configuration Loader

## Objective
Build a validated configuration subsystem that maps `dictation.toml` into typed dataclasses, enforces device/model constraints, and exposes CLI helpers for enumerating inputs and audio backends.

## Checklist

- [x] **Design configuration schema**
  - [x] Review the architecture to catalog fields for general, input, audio, transcriber, and injector settings.
  - [x] Define dataclasses (e.g., `GeneralConfig`, `InputConfig`, `AudioConfig`, `TranscriberConfig`, `InjectorConfig`, `AppConfig`) with defaults, type hints, and `__all__` exports.
  - [x] Provide factory helpers or `@dataclass` post-init hooks to normalize enums, paths, and optional collections.

- [x] **Implement loader utilities**
  - [x] Create `load_config(path: Path | None = None, *, env: Mapping[str, str] | None = None)` that resolves config search order (CLI flag, env var, default path) and reads TOML safely.
  - [x] Parse raw data using `tomllib` (stdlib) with a fallback import for `tomli` when necessary.
  - [x] Map parsed dictionaries into dataclasses, deriving defaults for omitted sections and coercing string values (e.g., device IDs, sample rates, directories).

- [x] **Validate configuration values**
  - [x] Implement evdev discovery helpers to locate the configured input device or raise a descriptive error when absent.
  - [x] Implement sounddevice discovery to confirm the audio input device exists and supports requested sample rate/channels.
  - [x] Validate transcriber parameters (model size, compute type, device) and injector command selections with friendly exception types.

- [x] **Expose CLI diagnostics**
  - [x] Add config module functions to enumerate available input and audio devices, returning structured data for CLI consumption.
  - [x] Extend Typer commands (`list-inputs`, `list-audio`, config path flag) to call the new helpers and render tabular or JSON output.
  - [x] Ensure CLI falls back gracefully when hardware probes fail (e.g., running in CI without devices).

- [x] **Testing and fixtures**
  - [x] Add unit tests covering default loading, explicit path loading, and environment override scenarios using temporary TOML fixtures.
  - [x] Mock evdev and sounddevice queries to exercise validation success/failure paths deterministically.
  - [x] Use `typer.testing.CliRunner` to verify CLI diagnostics commands and error messaging.

## Implementation Summary

**Status: ✅ COMPLETE**

All objectives for Step 3 have been successfully implemented and tested:

### Configuration Schema
- ✅ Five core dataclasses with sensible defaults: `InputConfig`, `AudioConfig`, `ModelConfig`, `InjectorConfig`, `TranscriptionConfig`
- ✅ Additional `GeneralConfig` for app-level settings
- ✅ Custom `ConfigError` exception for user-friendly error messages
- ✅ Proper `__all__` exports for clean public API

### Loader Utilities
- ✅ `Config.from_toml()` classmethod with full path resolution (CLI → env var → default locations)
- ✅ `load_config()` convenience wrapper
- ✅ TOML parsing with `tomllib` (3.11+) and `tomli` fallback
- ✅ Automatic type coercion and default handling

### Device Discovery & Validation
- ✅ `discover_input_devices()` - enumerates evdev devices
- ✅ `discover_audio_devices()` - queries sounddevice
- ✅ `validate_input_device()` - checks device existence/accessibility
- ✅ `validate_audio_device()` - validates audio capabilities
- ✅ Graceful fallback for CI/headless environments

### CLI Extensions
- ✅ `list-inputs` command with table/JSON output
- ✅ `list-audio` command with table/JSON output
- ✅ Config flag support on `run` and `dry-run` commands
- ✅ Full config validation before execution

### Testing
- ✅ 25 config module tests (config loading, validation, discovery)
- ✅ 13 CLI command tests (list-inputs, list-audio, run, dry-run)
- ✅ All tests passing (38/38)
- ✅ Code formatted with Black
- ✅ Push to GitHub completed

**Commit:** e6a912a
