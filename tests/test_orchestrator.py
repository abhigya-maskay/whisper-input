"""Tests for orchestrator state machine."""

import asyncio
import tempfile
from pathlib import Path
from unittest.mock import AsyncMock, Mock, patch

import pytest

from dictation_app.button_listener import ButtonEvent
from dictation_app.config import OrchestratorConfig
from dictation_app.orchestrator import Orchestrator, State, RecordingCancellationToken
from dictation_app.recorder import AudioRecorder
from dictation_app._types import TranscriptionResult, TranscriptionSegment


@pytest.fixture
def mock_button_listener():
    """Create mock ButtonListener."""
    mock = AsyncMock()
    mock.listen = AsyncMock()
    return mock


@pytest.fixture
def mock_recorder():
    """Create mock AudioRecorder."""
    return Mock(spec=AudioRecorder)


@pytest.fixture
def mock_transcriber():
    """Create mock Transcriber."""
    mock = AsyncMock()
    mock.transcribe = AsyncMock()
    mock.shutdown = AsyncMock()
    return mock


@pytest.fixture
def mock_injector():
    """Create mock Injector."""
    mock = AsyncMock()
    mock.inject_text = AsyncMock()
    return mock


@pytest.fixture
def orchestrator_config():
    """Create OrchestratorConfig for tests."""
    return OrchestratorConfig(
        max_retries=3,
        error_recovery_delay=0.1,
        silence_recovery_timeout=5.0,
        recording_timeout=300.0,
    )


@pytest.fixture
def orchestrator(mock_button_listener, mock_recorder, mock_transcriber, mock_injector, orchestrator_config):
    """Create Orchestrator with mocked components."""
    return Orchestrator(
        button_listener=mock_button_listener,
        recorder=mock_recorder,
        transcriber=mock_transcriber,
        injector=mock_injector,
        config=orchestrator_config,
    )


@pytest.fixture
def temp_audio_file():
    """Create temporary audio file for testing."""
    with tempfile.NamedTemporaryFile(mode="wb", suffix=".wav", delete=False) as f:
        f.write(b"RIFF" + b"\x00" * 36 + b"test")
        temp_path = Path(f.name)
    yield temp_path
    if temp_path.exists():
        temp_path.unlink()


class TestOrchestratorBasics:
    """Test basic orchestrator functionality."""

    def test_initialization(self, orchestrator):
        """Test orchestrator initialization."""
        assert orchestrator.state == State.IDLE
        assert orchestrator._shutdown_event is not None
        assert len(orchestrator._temp_files) == 0

    def test_initialization_with_default_config(self, mock_button_listener, mock_recorder, mock_transcriber, mock_injector):
        """Test orchestrator initialization with default config."""
        orch = Orchestrator(mock_button_listener, mock_recorder, mock_transcriber, mock_injector)
        assert orch.config is not None
        assert orch.config.max_retries == 3
        assert orch.config.error_recovery_delay == 1.0

    @pytest.mark.asyncio
    async def test_startup(self, orchestrator):
        """Test orchestrator startup."""
        await orchestrator.startup()
        assert orchestrator.state == State.IDLE

    @pytest.mark.asyncio
    async def test_button_press_starts_recording(self, orchestrator, mock_recorder, temp_audio_file):
        """Test button press triggers recording."""
        await orchestrator._on_button_pressed()

        mock_recorder.start.assert_called_once()
        assert orchestrator.state == State.RECORDING

    @pytest.mark.asyncio
    async def test_button_press_when_not_idle(self, orchestrator):
        """Test button press ignored when not in IDLE state."""
        orchestrator.state = State.RECORDING
        await orchestrator._on_button_pressed()

        orchestrator.recorder.start.assert_not_called()
        assert orchestrator.state == State.RECORDING

    @pytest.mark.asyncio
    async def test_button_release_stops_recording(self, orchestrator, mock_recorder, temp_audio_file):
        """Test button release stops recording."""
        orchestrator.state = State.RECORDING
        mock_recorder.stop.return_value = temp_audio_file

        await orchestrator._on_button_released()

        mock_recorder.stop.assert_called_once()
        assert orchestrator.state == State.TRANSCRIBING
        assert temp_audio_file in orchestrator._temp_files

        # Cancel the background transcription task to prevent warning
        if orchestrator._transcription_task and not orchestrator._transcription_task.done():
            orchestrator._transcription_task.cancel()
            try:
                await orchestrator._transcription_task
            except asyncio.CancelledError:
                pass

    @pytest.mark.asyncio
    async def test_button_release_when_not_recording(self, orchestrator):
        """Test button release ignored when not in RECORDING state."""
        orchestrator.state = State.IDLE
        await orchestrator._on_button_released()

        orchestrator.recorder.stop.assert_not_called()
        assert orchestrator.state == State.IDLE


class TestNominalFlow:
    """Test complete nominal flow."""

    @pytest.mark.asyncio
    async def test_full_pipeline_success(self, orchestrator, mock_button_listener, mock_recorder, 
                                         mock_transcriber, mock_injector, temp_audio_file):
        """Test complete flow: IDLE -> RECORDING -> TRANSCRIBING -> INJECTING -> IDLE."""
        mock_result = TranscriptionResult(
            text="Hello world",
            language="en",
            confidence=0.95,
            segments=[TranscriptionSegment(text="Hello world", start=0.0, end=1.0)],
        )
        mock_transcriber.transcribe.return_value = mock_result
        mock_recorder.stop.return_value = temp_audio_file

        async def button_events():
            yield ButtonEvent(pressed=True, timestamp=0.0)
            await asyncio.sleep(0.05)
            yield ButtonEvent(pressed=False, timestamp=0.1)
            await asyncio.sleep(0.5)

        # Mock the listen method to return async generator
        async def mock_listen():
            async for event in button_events():
                yield event

        mock_button_listener.listen = mock_listen

        run_task = asyncio.create_task(orchestrator.run())
        await asyncio.sleep(1.0)
        await orchestrator.shutdown()

        try:
            await asyncio.wait_for(run_task, timeout=2.0)
        except RuntimeError:
            pass  # Expected after shutdown

        mock_recorder.start.assert_called()
        mock_recorder.stop.assert_called()
        mock_transcriber.transcribe.assert_called()
        mock_injector.inject_text.assert_called_with("Hello world")

    @pytest.mark.asyncio
    async def test_transcription_and_injection_success(self, orchestrator, mock_transcriber, 
                                                      mock_injector, temp_audio_file):
        """Test transcription and injection stages."""
        mock_result = TranscriptionResult(
            text="Test message",
            language="en",
            confidence=0.9,
            segments=[],
        )
        mock_transcriber.transcribe.return_value = mock_result

        orchestrator.state = State.TRANSCRIBING
        await orchestrator._transcribe_and_inject(temp_audio_file)

        assert orchestrator.state == State.IDLE
        mock_transcriber.transcribe.assert_called_once()
        mock_injector.inject_text.assert_called_once_with("Test message")


class TestErrorHandling:
    """Test error handling and recovery."""

    @pytest.mark.asyncio
    async def test_recorder_start_failure(self, orchestrator, mock_recorder):
        """Test handling of recorder start failure."""
        mock_recorder.start.side_effect = RuntimeError("Recorder device unavailable")

        with pytest.raises(RuntimeError):
            await orchestrator._on_button_pressed()

        assert orchestrator.state == State.ERROR

    @pytest.mark.asyncio
    async def test_recorder_stop_failure(self, orchestrator, mock_recorder):
        """Test handling of recorder stop failure."""
        orchestrator.state = State.RECORDING
        mock_recorder.stop.side_effect = RuntimeError("Failed to save WAV")

        with pytest.raises(RuntimeError):
            await orchestrator._on_button_released()

        assert orchestrator.state == State.ERROR

    @pytest.mark.asyncio
    async def test_transcription_failure_retries(self, orchestrator, mock_transcriber, temp_audio_file):
        """Test transcription failure with retry logic."""
        orchestrator.state = State.TRANSCRIBING
        
        # First attempt fails, second succeeds
        mock_result = TranscriptionResult(
            text="Success",
            language="en",
            confidence=0.9,
            segments=[],
        )
        mock_transcriber.transcribe.side_effect = [
            RuntimeError("Transcription failed"),
            mock_result,
        ]
        orchestrator.injector = AsyncMock()
        orchestrator.injector.inject_text = AsyncMock()

        await orchestrator._transcribe_and_inject(temp_audio_file)

        assert orchestrator.state == State.IDLE
        assert mock_transcriber.transcribe.call_count == 2

    @pytest.mark.asyncio
    async def test_transcription_timeout_recovery(self, orchestrator, mock_transcriber, temp_audio_file):
        """Test transcription timeout handling."""
        orchestrator.state = State.TRANSCRIBING
        mock_transcriber.transcribe.side_effect = asyncio.TimeoutError()

        await orchestrator._transcribe_and_inject(temp_audio_file)

        # After max retries and error recovery, state should be IDLE
        assert orchestrator.state == State.IDLE
        assert mock_transcriber.transcribe.call_count == orchestrator.config.max_retries

    @pytest.mark.asyncio
    async def test_injection_failure_retries(self, orchestrator, mock_injector, mock_transcriber, temp_audio_file):
        """Test injection failure with retry logic."""
        orchestrator.state = State.TRANSCRIBING
        
        mock_result = TranscriptionResult(
            text="Test",
            language="en",
            confidence=0.9,
            segments=[],
        )
        mock_transcriber.transcribe.return_value = mock_result
        
        # First attempt fails, second succeeds
        mock_injector.inject_text.side_effect = [
            RuntimeError("Injection failed"),
            None,
        ]

        await orchestrator._transcribe_and_inject(temp_audio_file)

        assert orchestrator.state == State.IDLE
        assert mock_injector.inject_text.call_count == 2

    @pytest.mark.asyncio
    async def test_error_recovery_delay(self, orchestrator):
        """Test error recovery waits configured delay."""
        orchestrator.state = State.ERROR
        error = RuntimeError("Test error")

        start = asyncio.get_event_loop().time()
        await orchestrator._error_recovery(error)
        elapsed = asyncio.get_event_loop().time() - start

        assert orchestrator.state == State.IDLE
        assert elapsed >= orchestrator.config.error_recovery_delay

    @pytest.mark.asyncio
    async def test_empty_transcription_retries(self, orchestrator, mock_transcriber, temp_audio_file):
        """Test retry on empty transcription result."""
        orchestrator.state = State.TRANSCRIBING
        
        # First returns empty, second succeeds
        mock_result = TranscriptionResult(
            text="Success",
            language="en",
            confidence=0.9,
            segments=[],
        )
        mock_transcriber.transcribe.side_effect = [
            TranscriptionResult(text="", language="en", confidence=0.0, segments=[]),
            mock_result,
        ]
        orchestrator.injector = AsyncMock()
        orchestrator.injector.inject_text = AsyncMock()

        await orchestrator._transcribe_and_inject(temp_audio_file)

        assert orchestrator.state == State.IDLE
        assert mock_transcriber.transcribe.call_count == 2


class TestShutdown:
    """Test graceful shutdown."""

    @pytest.mark.asyncio
    async def test_shutdown_closes_resources(self, orchestrator, mock_recorder, mock_transcriber):
        """Test shutdown closes all resources."""
        with tempfile.NamedTemporaryFile(mode="wb", suffix=".wav", delete=False) as f:
            temp_path = Path(f.name)

        orchestrator._temp_files.add(temp_path)

        await orchestrator.shutdown()

        assert orchestrator.state == State.SHUTDOWN
        mock_recorder.close.assert_called_once()
        mock_transcriber.shutdown.assert_called_once()
        assert not temp_path.exists()

    @pytest.mark.asyncio
    async def test_shutdown_cancels_tasks(self, orchestrator):
        """Test shutdown cancels active tasks."""
        # Create a real task that can be cancelled
        async def dummy():
            await asyncio.sleep(10)
        
        task = asyncio.create_task(dummy())
        orchestrator._transcription_task = task

        await orchestrator.shutdown()

        # Task should be cancelled
        assert task.cancelled() or task.done()

    @pytest.mark.asyncio
    async def test_shutdown_handles_task_cancellation_timeout(self, orchestrator):
        """Test shutdown handles task cancellation timeout."""
        async def slow_task():
            await asyncio.sleep(10.0)

        task = asyncio.create_task(slow_task())
        orchestrator._transcription_task = task

        # Should not raise even if task takes time to cancel
        await orchestrator.shutdown()
        assert orchestrator.state == State.SHUTDOWN


class TestRecorderEdgeCases:
    """Test recorder edge cases."""

    @pytest.mark.asyncio
    async def test_audio_file_not_created(self, orchestrator, mock_recorder):
        """Test handling of missing audio file."""
        orchestrator.state = State.RECORDING
        mock_recorder.stop.return_value = Path("/nonexistent/file.wav")

        with pytest.raises(RuntimeError, match="Audio file not created"):
            await orchestrator._on_button_released()

        assert orchestrator.state == State.ERROR

    @pytest.mark.asyncio
    async def test_empty_audio_file(self, orchestrator, mock_recorder):
        """Test handling of empty audio file."""
        orchestrator.state = State.RECORDING
        
        with tempfile.NamedTemporaryFile(mode="wb", suffix=".wav", delete=False) as f:
            empty_path = Path(f.name)

        mock_recorder.stop.return_value = empty_path

        with pytest.raises(RuntimeError, match="Audio file is empty"):
            await orchestrator._on_button_released()

        assert orchestrator.state == State.ERROR
        empty_path.unlink()


class TestMaxRetriesExhaustion:
    """Test behavior when max retries exhausted."""

    @pytest.mark.asyncio
    async def test_max_retries_exhausted(self, orchestrator, mock_transcriber, temp_audio_file):
        """Test orchestrator transitions to IDLE after max retries and recovery."""
        orchestrator.state = State.TRANSCRIBING
        mock_transcriber.transcribe.side_effect = RuntimeError("Consistent failure")

        await orchestrator._transcribe_and_inject(temp_audio_file)

        # After max retries and error recovery, state should be IDLE
        assert orchestrator.state == State.IDLE
        assert mock_transcriber.transcribe.call_count == orchestrator.config.max_retries


class TestRecordingCancellationToken:
    """Test recording cancellation token."""

    def test_token_initialization(self):
        """Test cancellation token initializes in non-cancelled state."""
        token = RecordingCancellationToken()
        assert not token.is_cancelled()

    def test_token_cancel(self):
        """Test cancelling the token."""
        token = RecordingCancellationToken()
        assert not token.is_cancelled()

        token.cancel()
        assert token.is_cancelled()

    def test_token_reset(self):
        """Test resetting a cancelled token."""
        token = RecordingCancellationToken()
        token.cancel()
        assert token.is_cancelled()

        token.reset()
        assert not token.is_cancelled()

    @pytest.mark.asyncio
    async def test_button_press_resets_token(self, orchestrator, mock_recorder, temp_audio_file):
        """Test button press resets cancellation token."""
        orchestrator._recording_cancel_token.cancel()
        assert orchestrator._recording_cancel_token.is_cancelled()

        await orchestrator._on_button_pressed()

        # Token should be reset
        assert not orchestrator._recording_cancel_token.is_cancelled()

    @pytest.mark.asyncio
    async def test_button_release_respects_cancelled_token(self, orchestrator, mock_recorder):
        """Test button release skips processing if token is cancelled."""
        orchestrator.state = State.RECORDING
        orchestrator._recording_cancel_token.cancel()

        await orchestrator._on_button_released()

        # Should transition to IDLE without processing
        assert orchestrator.state == State.IDLE
        mock_recorder.stop.assert_not_called()

    @pytest.mark.asyncio
    async def test_error_recovery_cancels_token(self, orchestrator):
        """Test error recovery cancels the recording token."""
        orchestrator.state = State.ERROR
        orchestrator._recording_cancel_token.reset()

        error = RuntimeError("Test error")
        await orchestrator._error_recovery(error)

        # Token should be cancelled
        assert orchestrator._recording_cancel_token.is_cancelled()
        # State should recover to IDLE
        assert orchestrator.state == State.IDLE

    @pytest.mark.asyncio
    async def test_shutdown_cancels_token(self, orchestrator, mock_recorder, mock_transcriber):
        """Test shutdown cancels the recording token."""
        orchestrator._recording_cancel_token.reset()
        assert not orchestrator._recording_cancel_token.is_cancelled()

        await orchestrator.shutdown()

        # Token should be cancelled
        assert orchestrator._recording_cancel_token.is_cancelled()


class TestStartupInitialization:
    """Test orchestrator startup initialization."""

    @pytest.mark.asyncio
    async def test_startup_resets_token(self, orchestrator):
        """Test startup resets cancellation token."""
        orchestrator._recording_cancel_token.cancel()
        assert orchestrator._recording_cancel_token.is_cancelled()

        await orchestrator.startup()

        # Token should be reset
        assert not orchestrator._recording_cancel_token.is_cancelled()

    @pytest.mark.asyncio
    async def test_startup_primes_button_listener(self, orchestrator, mock_button_listener):
        """Test startup attempts to prime button listener."""
        # Mock the async context manager
        mock_button_listener.__aenter__ = AsyncMock(return_value=mock_button_listener)
        mock_button_listener.__aexit__ = AsyncMock(return_value=None)

        await orchestrator.startup()

        # Button listener context manager should have been used
        mock_button_listener.__aenter__.assert_called_once()
        mock_button_listener.__aexit__.assert_called_once()

    @pytest.mark.asyncio
    async def test_startup_handles_button_listener_failure(self, orchestrator, mock_button_listener):
        """Test startup handles button listener priming failure."""
        # Mock the async context manager to raise
        mock_button_listener.__aenter__ = AsyncMock(
            side_effect=RuntimeError("Cannot open device")
        )
        mock_button_listener.__aexit__ = AsyncMock(return_value=None)

        with pytest.raises(RuntimeError, match="Failed to prime button listener"):
            await orchestrator.startup()

    @pytest.mark.asyncio
    async def test_startup_logs_config(self, orchestrator, caplog):
        """Test startup logs configuration."""
        import logging
        caplog.set_level(logging.DEBUG)
        await orchestrator.startup()
        assert "Orchestrator startup complete" in caplog.text
        assert "max_retries" in caplog.text
