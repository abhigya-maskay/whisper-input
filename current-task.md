# Current Task: Step 3 – Implement Configuration Loader

## Objective
Build a validated configuration subsystem that maps `dictation.toml` into typed dataclasses, enforces device/model constraints, and exposes CLI helpers for enumerating inputs and audio backends.

## Checklist

- [ ] **Design configuration schema**
  - [ ] Review the architecture to catalog fields for general, input, audio, transcriber, and injector settings.
  - [ ] Define dataclasses (e.g., `GeneralConfig`, `InputConfig`, `AudioConfig`, `TranscriberConfig`, `InjectorConfig`, `AppConfig`) with defaults, type hints, and `__all__` exports.
  - [ ] Provide factory helpers or `@dataclass` post-init hooks to normalize enums, paths, and optional collections.

- [ ] **Implement loader utilities**
  - [ ] Create `load_config(path: Path | None = None, *, env: Mapping[str, str] | None = None)` that resolves config search order (CLI flag, env var, default path) and reads TOML safely.
  - [ ] Parse raw data using `tomllib` (stdlib) with a fallback import for `tomli` when necessary.
  - [ ] Map parsed dictionaries into dataclasses, deriving defaults for omitted sections and coercing string values (e.g., device IDs, sample rates, directories).

- [ ] **Validate configuration values**
  - [ ] Implement evdev discovery helpers to locate the configured input device or raise a descriptive error when absent.
  - [ ] Implement sounddevice discovery to confirm the audio input device exists and supports requested sample rate/channels.
  - [ ] Validate transcriber parameters (model size, compute type, device) and injector command selections with friendly exception types.

- [ ] **Expose CLI diagnostics**
  - [ ] Add config module functions to enumerate available input and audio devices, returning structured data for CLI consumption.
  - [ ] Extend Typer commands (`list-inputs`, `list-audio`, config path flag) to call the new helpers and render tabular or JSON output.
  - [ ] Ensure CLI falls back gracefully when hardware probes fail (e.g., running in CI without devices).

- [ ] **Testing and fixtures**
  - [ ] Add unit tests covering default loading, explicit path loading, and environment override scenarios using temporary TOML fixtures.
  - [ ] Mock evdev and sounddevice queries to exercise validation success/failure paths deterministically.
  - [ ] Use `typer.testing.CliRunner` to verify CLI diagnostics commands and error messaging.
