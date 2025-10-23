"""Central async state machine orchestrating all components."""

import asyncio
import logging
import tempfile
from enum import Enum
from pathlib import Path

from dictation_app.button_listener import ButtonListener, ButtonEvent
from dictation_app.config import OrchestratorConfig
from dictation_app.injector import Injector
from dictation_app.recorder import AudioRecorder
from dictation_app.transcriber import Transcriber

logger = logging.getLogger(__name__)


class RecordingCancellationToken:
    """Token for gracefully aborting in-flight recordings.

    Provides a simple cancellation mechanism that can be checked by the recorder
    to abort recording cleanly during errors or shutdown.
    """

    def __init__(self):
        """Initialize cancellation token in non-cancelled state."""
        self._cancelled = False

    def cancel(self) -> None:
        """Mark token as cancelled."""
        self._cancelled = True

    def is_cancelled(self) -> bool:
        """Check if token is cancelled.

        Returns:
            True if cancelled, False otherwise
        """
        return self._cancelled

    def reset(self) -> None:
        """Reset token to non-cancelled state."""
        self._cancelled = False


class State(Enum):
    """Orchestrator state."""

    IDLE = "idle"
    RECORDING = "recording"
    TRANSCRIBING = "transcribing"
    INJECTING = "injecting"
    ERROR = "error"
    SHUTDOWN = "shutdown"


class Orchestrator:
    """Coordinates button listener, recorder, transcriber, and injector.

    Central state machine handling lifecycle events, timeouts, and error propagation.
    """

    def __init__(
        self,
        button_listener: ButtonListener,
        recorder: AudioRecorder,
        transcriber: Transcriber,
        injector: Injector,
        config: OrchestratorConfig | None = None,
    ):
        """Initialize orchestrator with components.

        Args:
            button_listener: ButtonListener instance
            recorder: AudioRecorder instance
            transcriber: Transcriber instance
            injector: Injector instance
            config: OrchestratorConfig for state machine parameters
        """
        self.button_listener = button_listener
        self.recorder = recorder
        self.transcriber = transcriber
        self.injector = injector
        self.config = config or OrchestratorConfig()

        self.state = State.IDLE
        self._shutdown_event = asyncio.Event()
        self._recording_cancel_token = RecordingCancellationToken()
        self._recording_task: asyncio.Task | None = None
        self._transcription_task: asyncio.Task | None = None
        self._injection_task: asyncio.Task | None = None
        self._temp_files: set[Path] = set()
        self._last_error: Exception | None = None

        logger.info("Orchestrator initialized in IDLE state")

    async def startup(self) -> None:
        """Initialize orchestrator and register callbacks.

        Primes button listener, resets cancellation token, and configures logging hooks.
        Ensures event handling infrastructure is ready before entering the event loop.

        Raises:
            RuntimeError: If startup fails
        """
        logger.info("Orchestrator startup")
        try:
            # Initialize/reset cancellation token
            self._recording_cancel_token.reset()
            logger.debug("Recording cancellation token reset")

            # Prime button listener device (ensure it can be opened)
            try:
                async with self.button_listener:
                    logger.debug("Button listener device primed successfully")
            except Exception as e:
                logger.warning("Failed to prime button listener: %s", e)
                raise RuntimeError(f"Failed to prime button listener: {e}") from e

            logger.debug(
                "Config: max_retries=%d, error_recovery_delay=%.1fs, "
                "recording_timeout=%.1fs",
                self.config.max_retries,
                self.config.error_recovery_delay,
                self.config.recording_timeout,
            )
            logger.info("Orchestrator startup complete")
        except Exception as e:
            logger.error("Startup failed: %s", e, exc_info=True)
            raise RuntimeError(f"Orchestrator startup failed: {e}") from e

    async def run(self) -> None:
        """Main event loop coordinating all components.

        Listens for button events and coordinates recording -> transcription -> injection pipeline.
        Handles errors and recovers to IDLE state.

        Raises:
            RuntimeError: On unrecoverable failure
        """
        logger.info("Orchestrator event loop starting")
        await self.startup()

        try:
            async for button_event in self.button_listener.listen():
                if self._shutdown_event.is_set():
                    logger.info("Shutdown signal received, exiting event loop")
                    self.state = State.SHUTDOWN
                    break

                try:
                    if button_event.pressed:
                        await self._on_button_pressed()
                    else:
                        await self._on_button_released()
                except Exception as e:
                    logger.error("Error in button event handler: %s", e, exc_info=True)
                    self._last_error = e
                    await self._error_recovery(e)

        except asyncio.CancelledError:
            logger.info("Orchestrator cancelled")
            await self.shutdown()
            raise
        except Exception as e:
            logger.error("Orchestrator error: %s", e, exc_info=True)
            self._last_error = e
            self.state = State.ERROR
            await self.shutdown()
            raise RuntimeError(f"Orchestrator failed: {e}") from e

    async def shutdown(self) -> None:
        """Gracefully shut down orchestrator and clean up resources.

        Cancels recording via token, cancels active tasks, closes listeners,
        and cleans temporary files.
        """
        logger.info("Orchestrator shutdown starting")
        self._shutdown_event.set()
        self.state = State.SHUTDOWN

        # Cancel in-flight recording via token
        logger.debug("Cancelling in-flight recording via token")
        self._recording_cancel_token.cancel()

        # Cancel active tasks
        for task_ref, task_name in [
            (self._recording_task, "recording"),
            (self._transcription_task, "transcription"),
            (self._injection_task, "injection"),
        ]:
            if task_ref and not task_ref.done():
                logger.debug("Cancelling %s task", task_name)
                task_ref.cancel()
                try:
                    await asyncio.wait_for(task_ref, timeout=5.0)
                except (asyncio.CancelledError, asyncio.TimeoutError):
                    pass

        # Stop recorder if recording
        if self.recorder:
            try:
                self.recorder.close()
                logger.debug("Recorder closed")
            except Exception as e:
                logger.warning("Error closing recorder: %s", e)

        # Shutdown transcriber
        if self.transcriber:
            try:
                await self.transcriber.shutdown()
                logger.debug("Transcriber shut down")
            except Exception as e:
                logger.warning("Error shutting down transcriber: %s", e)

        # Clean temporary files
        for temp_file in self._temp_files:
            try:
                if temp_file.exists():
                    temp_file.unlink()
                    logger.debug("Deleted temporary file: %s", temp_file)
            except Exception as e:
                logger.warning("Error deleting temp file %s: %s", temp_file, e)
        self._temp_files.clear()

        logger.info("Orchestrator shutdown complete")

    async def _on_button_pressed(self) -> None:
        """Handle button press event: transition IDLE -> RECORDING.

        Starts audio recorder with cancellation token and logs state change.

        Raises:
            RuntimeError: If recording cannot start
        """
        if self.state != State.IDLE:
            logger.warning(
                "Button pressed while in %s state, ignoring", self.state.value
            )
            return

        logger.info("State transition: IDLE -> RECORDING")
        self.state = State.RECORDING

        try:
            # Reset cancellation token before starting new recording
            self._recording_cancel_token.reset()
            self.recorder.start(token=self._recording_cancel_token)
            logger.debug("Audio recorder started with cancellation token")
        except RuntimeError as e:
            logger.error("Failed to start recorder: %s", e)
            self.state = State.ERROR
            raise

    async def _on_button_released(self) -> None:
        """Handle button release event: transition RECORDING -> TRANSCRIBING.

        Stops recorder, retrieves WAV path, handles edge cases (silence, no audio).
        Checks cancellation token to allow graceful abort during shutdown/error.

        Raises:
            RuntimeError: On error conditions
        """
        if self.state != State.RECORDING:
            logger.warning(
                "Button released while in %s state, ignoring", self.state.value
            )
            return

        # Check if recording was cancelled (e.g., during error recovery or shutdown)
        if self._recording_cancel_token.is_cancelled():
            logger.info("Recording cancelled, skipping transcription")
            self.state = State.IDLE
            return

        logger.info("State transition: RECORDING -> TRANSCRIBING")
        self.state = State.TRANSCRIBING

        try:
            audio_path = self.recorder.stop()
            self._temp_files.add(audio_path)
            logger.debug("Audio recording stopped and saved to %s", audio_path)

            # Verify audio file exists and has content
            if not audio_path.exists():
                raise RuntimeError(f"Audio file not created: {audio_path}")

            file_size = audio_path.stat().st_size
            if file_size == 0:
                raise RuntimeError("Audio file is empty")

            logger.debug("Audio file size: %d bytes", file_size)

            # Spawn transcription task
            self._transcription_task = asyncio.create_task(
                self._transcribe_and_inject(audio_path)
            )

        except RuntimeError as e:
            logger.error("Failed to stop recording: %s", e)
            self.state = State.ERROR
            raise

    async def _transcribe_and_inject(self, audio_path: Path) -> None:
        """Transcribe audio and inject text with retry strategy.

        Transcription and injection are retried on failure with exponential backoff.

        Args:
            audio_path: Path to WAV file

        Raises:
            RuntimeError: On persistent failure after max retries
        """
        # Transcription stage with retries
        text = None
        retry_count = 0
        last_error = None

        while retry_count < self.config.max_retries:
            try:
                logger.debug(
                    "Transcribing audio (attempt %d/%d)",
                    retry_count + 1,
                    self.config.max_retries,
                )
                result = await self.transcriber.transcribe(audio_path)
                text = result.text.strip()

                if not text:
                    logger.warning("Transcription returned empty text")
                    retry_count += 1
                    if retry_count < self.config.max_retries:
                        await asyncio.sleep(0.5)
                    continue

                logger.info("Transcription successful: %d characters", len(text))
                logger.debug("Transcribed text: %s", text)
                break  # Success, exit retry loop

            except (RuntimeError, TimeoutError, Exception) as e:
                retry_count += 1
                last_error = e
                logger.warning(
                    "Transcription attempt %d/%d failed (%s: %s)",
                    retry_count,
                    self.config.max_retries,
                    type(e).__name__,
                    e,
                )

                if retry_count < self.config.max_retries:
                    # Exponential backoff: 0.5s, 1.0s, 2.0s, ...
                    wait_time = 0.5 * (2 ** (retry_count - 1))
                    logger.debug("Waiting %.1fs before retry", wait_time)
                    await asyncio.sleep(wait_time)

        # If transcription failed after all retries
        if not text:
            logger.error(
                "Failed to transcribe after %d attempts: %s",
                retry_count,
                last_error,
            )
            self.state = State.ERROR
            await self._error_recovery(last_error or RuntimeError("Transcription failed"))
            return

        # Injection stage with retries mirroring transcription behaviour
        logger.info("State transition: TRANSCRIBING -> INJECTING")
        self.state = State.INJECTING

        injection_attempt = 0
        last_injection_error: Exception | None = None

        while injection_attempt < self.config.max_retries:
            injection_attempt += 1
            try:
                logger.debug(
                    "Injecting text (attempt %d/%d)",
                    injection_attempt,
                    self.config.max_retries,
                )
                await self.injector.inject_text(text)
                logger.info("Text injection successful")
                logger.info("State transition: INJECTING -> IDLE")
                self.state = State.IDLE
                return
            except (RuntimeError, TimeoutError, Exception) as e:
                last_injection_error = e
                logger.warning(
                    "Text injection attempt %d/%d failed (%s: %s)",
                    injection_attempt,
                    self.config.max_retries,
                    type(e).__name__,
                    e,
                )
                if injection_attempt < self.config.max_retries:
                    wait_time = 0.5 * (2 ** (injection_attempt - 1))
                    logger.debug("Waiting %.1fs before retrying injection", wait_time)
                    await asyncio.sleep(wait_time)

        # All injection attempts failed
        assert last_injection_error is not None
        logger.error(
            "Text injection failed after %d attempts: %s: %s",
            self.config.max_retries,
            type(last_injection_error).__name__,
            last_injection_error,
        )
        self.state = State.ERROR
        await self._error_recovery(last_injection_error)

    async def _error_recovery(self, error: Exception) -> None:
        """Recover from error state by waiting and transitioning to IDLE.

        Logs error details, cancels in-flight recording, waits configurable delay,
        then attempts to reset.

        Args:
            error: The exception that triggered recovery
        """
        logger.warning(
            "Error recovery: waiting %.1fs before IDLE (error: %s: %s)",
            self.config.error_recovery_delay,
            type(error).__name__,
            error,
        )

        # Cancel in-flight recording to prevent processing during recovery
        logger.debug("Cancelling recording during error recovery")
        self._recording_cancel_token.cancel()

        await asyncio.sleep(self.config.error_recovery_delay)

        if self.state == State.ERROR:
            logger.info("State transition: ERROR -> IDLE")
            self.state = State.IDLE
