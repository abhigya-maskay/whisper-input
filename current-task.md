# Current Task: Step 6 – Integrate Faster Whisper Transcriber

- [x] Audit existing configuration (`config.py`) for transcriber options (model path, device, compute type, beam size) and extend schemas/defaults as needed
- [x] Design transcriber module structure (e.g., `transcriber.py`) with class encapsulating model reference, executor, and normalization utilities
- [x] Implement lazy `WhisperModel` initialization triggered on first transcription, using configured model directory and logging duration
- [x] Configure compute target selection (`cpu`, `cuda`, `auto`) and compute type enforcement, mapping config values to FasterWhisper arguments
- [x] Establish reusable thread pool or `asyncio.to_thread` strategy to offload `WhisperModel.transcribe` without blocking event loop
- [x] Implement audio ingestion helper converting temporary WAV path into FasterWhisper-compatible input and handling sample rate discrepancies
- [x] Add transcription entry point returning primary text plus optional segments, surfacing descriptive `RuntimeError` on model failures
- [x] Implement text post-processing pipeline (strip, lowercase/capitalize rules, punctuation normalization) with configuration toggles if applicable
- [x] Ensure proper cleanup for executors/model resources on shutdown, aligning with orchestrator lifecycle expectations
- [x] Write unit tests mocking `WhisperModel` to verify lazy loading, executor invocation, and error propagation without importing real weights
- [x] Create tests covering normalization logic with diverse sample inputs and ensure deterministic outputs
- [x] Prepare fixture WAV asset or factory generating synthetic audio for integration tests without external dependencies
