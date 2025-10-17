# Current Task: Step 8 – Assemble Orchestrator

- [x] Review existing listener, recorder, transcriber, and injector APIs to confirm call signatures, async behavior, and error contracts
- [x] Draft orchestrator architecture outlining states (idle, recording, transcribing, injecting, error, shutdown) and transitions, noting timeout boundaries from config
- [x] Define orchestrator configuration dataclasses or sections to capture state-machine parameters (timeouts, retry limits, logging levels) and ensure defaults
- [x] Implement orchestrator class skeleton with dependency injection for listener/recorder/transcriber/injector instances and shared asyncio loop context
- [x] Implement async startup routine that registers button listener callbacks, primes shared queues, and configures logging/metrics hooks
- [x] Implement recording cycle handler: on press -> start recorder, maintain cancellation token; on release -> stop recorder, retrieve WAV path, handle silence/no-audio conditions
- [x] Implement transcription stage using executor-backed task, apply normalization, timeout handling, and propagate meaningful errors to orchestrator state
- [x] Implement text injection stage with retry strategy, clipboard fallback coordination, and structured success/failure signaling
- [x] Add error management paths that map component exceptions to orchestrator states, logging severity, and user-facing notifications when available
- [x] Implement graceful shutdown routine closing listeners, cancelling pending asyncio tasks, cleaning temporary files, and ensuring recorder/transcriber resources freed
- [x] Integrate logging at state boundaries and decision points, honoring verbosity settings, and wire optional notification hooks (e.g., sound/desktop placeholder)
- [x] Write pytest-asyncio tests using fakes/mocks for all components to validate nominal flow, timeout handling, and failure recovery logic

## Follow-up Action Items

- [x] Expand `startup()` to register listener callbacks or spawn the button-listener task so that event handling is primed before `run()` iterates, and initialize any shared queues or hooks needed for metrics/logging.
- [x] Introduce a recording cancellation token tracked during `_on_button_pressed()` and honored in `_on_button_released()`/shutdown to guarantee in-flight recordings can be aborted cleanly during errors or shutdown.
