# Current Task: Step 7 – Create Text Injector

- [x] Review `config.py` and related schemas to ensure injector configuration covers binary selection (`wtype`/`ydotool`), clipboard fallback toggle, dry-run option, and timeouts; extend defaults if missing
- [x] Outline injector module structure (e.g., `injector.py`) with a class exposing `inject_text` and dependency hooks for subprocess execution and clipboard utilities
- [x] Implement command resolution that builds argument lists for `wtype` (default) and `ydotool`, including support for configurable typing delays and newline behavior when specified
- [x] Add logic to detect Wayland vs Ydotool path requirements and surface descriptive configuration errors when binaries are unavailable or not executable
- [x] Implement dry-run mode to log intended actions without calling external binaries
- [x] Integrate primary execution path that streams text via the chosen typing tool using `asyncio.to_thread` or a dedicated executor to avoid blocking the event loop
- [x] Capture subprocess results, enforcing timeouts and raising `RuntimeError` with stderr context when exit codes are non-zero or timeouts occur
- [x] Implement clipboard fallback by piping text into `wl-copy`, validating exit success, and retrying the typing command once after the clipboard update
- [x] Provide structured error handling that distinguishes between command-not-found, timeout, and execution failure, mapping them to actionable messages for the orchestrator
- [x] Expose a dry-run friendly logging pathway that includes the resolved command, fallback behavior, and final status
- [x] Add unit tests mocking subprocess invocations to verify success path, failure path with fallback activation, dry-run behavior, and timeout handling without invoking real binaries
- [x] Add tests ensuring configuration errors are raised when required binaries are unavailable and that clipboard fallback is skipped when disabled by config
- [x] Verify injector module integrates with orchestrator interfaces (placeholders acceptable) and document expected async usage in code comments if necessary
