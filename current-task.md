# Current Task: Step 4 – Build Button Listener Module

- [x] Deliver a production-ready button listener that integrates with evdev, provides debounced press/release events, and exposes device discovery helpers to feed configuration and CLI diagnostics.
- [x] Introduce async generator `_iter_key_events` wrapping `evdev.InputDevice.async_read_loop()` filtered to `EV_KEY` events for the configured `key_code`.
- [x] Map raw events into `ButtonEvent` instances containing `pressed` and `timestamp` metadata.
- [x] Maintain last event timestamp/state and suppress duplicates within `debounce_ms`.
- [x] Emit `pressed=True` on key-down and `pressed=False` on key-up after debounce window, capturing hold behavior.
- [x] Add async context manager support to open/close `InputDevice` safely.
- [x] Ensure `listen()` handles cancellation, closes descriptors, and logs debug details.
- [x] Enumerate `/dev/input/by-id`, resolve symlinks, and return `{path: name}` mapping.
- [x] Handle missing directory or permission errors with logged warnings aligned with config discovery utilities.
- [x] Provide `ButtonListener.from_config(config: InputConfig)` for orchestrator integration.
- [x] Use `pytest-asyncio` with patched `async_read_loop` emitting canned events covering press/release, rapid toggles, and cancellation cleanup.
- [x] Patch `Path.iterdir` and `evdev.InputDevice` to simulate discovery, ensuring resolved paths and friendly names.
- [x] Assert permission/missing-device scenarios raise `RuntimeError` with clear messaging while logging warnings without crashing tests.
- [x] Raise `RuntimeError` for device failures to align with orchestrator expectations.
- [x] Avoid exposing raw `evdev.InputEvent`, relying on logs for debugging to keep API minimal.
- [x] Log event emissions at DEBUG level to avoid noisy output in production.

## Summary

**Status: ✅ COMPLETE**

Step 4 button listener implementation is production-ready with comprehensive test coverage:

### Implementation (dictation_app/button_listener.py)
- ✅ `ButtonListener` class with async context manager protocol
- ✅ `_iter_key_events()` async generator filtering for `EV_KEY` events by key code
- ✅ `ButtonEvent` dataclass with `pressed` bool and `timestamp` metadata
- ✅ Debouncing logic maintaining `_last_event_time` and `_last_pressed_state`
- ✅ `listen()` async generator emitting debounced events with cancellation handling
- ✅ `from_config()` factory method for orchestrator integration
- ✅ `list_devices()` static method enumerating `/dev/input/by-id` with symlink resolution
- ✅ Comprehensive error handling raising `RuntimeError` with clear messaging
- ✅ DEBUG-level logging for event emissions and device operations

### Testing (tests/test_button_listener.py)
- ✅ 14 comprehensive tests covering:
  - Basic event emission and debouncing
  - Rapid toggle suppression within debounce window
  - Key code validation with invalid keycodes
  - Device access errors (OSError, PermissionError)
  - Cancellation cleanup and resource management
  - Device discovery with permission errors and mixed scenarios
  - Context manager protocol compliance
- ✅ All tests use `pytest-asyncio` with proper async mocking
- ✅ Tests use patched `Path.iterdir` and `InputDevice` for deterministic scenarios
- ✅ All 14 tests passing
- ✅ Zero linting issues (ruff clean)
