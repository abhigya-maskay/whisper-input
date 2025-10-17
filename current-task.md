# Current Task: Step 9 – Wire CLI Entry Point

- [x] Review existing Typer application structure (`dictation_app/main.py`) and confirm how configuration loading and dependency injection are expected to work.
- [x] Ensure `dictation_app.config` exposes helper utilities for diagnostics (list inputs/audio) that can be invoked from the CLI layer without circular imports.
- [x] Design the CLI command layout: primary `run` command for orchestrator startup plus diagnostic subcommands for listing inputs, audio devices, and other helpers.
- [x] Implement `run` command wiring that loads configuration (including overrides passed via CLI options), instantiates orchestrator dependencies, and launches the orchestrator lifecycle within an asyncio event loop.
- [x] Add CLI options for common configuration overrides (e.g., path to config file, device identifiers, model selection) and ensure they merge cleanly with defaults.
- [x] Expose diagnostic commands that reuse existing config/device helper functions and present structured output, handling errors with informative messages and exit codes.
- [x] Hook Typer callbacks to propagate logging verbosity flags and dry-run settings down to orchestrator and component constructors.
- [x] Update `__main__` entry point so `python -m dictation_app` and the installed console script both invoke the Typer app correctly.
- [x] Modify `flake.nix` (and any supporting packaging files) to add a runnable package/executable exposing the CLI as `packages.${system}.dictation-app`.
- [x] Validate the CLI locally (`--help`, diagnostics commands, sample `run` invocation) and add or update tests to cover command wiring if feasible.
