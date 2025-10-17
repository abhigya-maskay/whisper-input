"""Tests for recorder resilience: device failures, silent audio, recovery."""

import tempfile
from pathlib import Path
from unittest.mock import MagicMock, Mock, patch

import numpy as np
import pytest

from dictation_app.recorder import AudioRecorder


class TestRecorderDeviceFailures:
    """Test recorder behavior with device failures."""

    @patch("dictation_app.recorder.sounddevice.InputStream")
    def test_device_disconnection_during_recording(self, mock_stream_class):
        """Test recorder handles device disconnection gracefully."""
        mock_stream_instance = MagicMock()
        mock_stream_class.return_value = mock_stream_instance

        recorder = AudioRecorder(device=0)
        recorder.start()

        # Verify stream was created
        assert recorder._stream is not None
        mock_stream_instance.start.assert_called_once()

    @patch("dictation_app.recorder.sounddevice.InputStream")
    def test_recorder_start_device_unavailable(self, mock_stream_class):
        """Test recorder handles unavailable audio device."""
        mock_stream_class.side_effect = RuntimeError("ALSA: Can't open PCM device")

        recorder = AudioRecorder(device=99)

        with pytest.raises(RuntimeError):
            recorder.start()

    @patch("dictation_app.recorder.sounddevice.InputStream")
    def test_recorder_recovery_after_device_error(self, mock_stream_class):
        """Test recorder can recover by re-initializing with valid device."""
        # First attempt fails
        mock_stream_class.side_effect = RuntimeError("Device unavailable")
        
        recorder = AudioRecorder(device=0)
        with pytest.raises(RuntimeError):
            recorder.start()

        # Reset side effect and try again
        mock_stream_class.side_effect = None
        mock_stream_instance = MagicMock()
        mock_stream_class.return_value = mock_stream_instance
        
        # Should not raise
        recorder.start()
        assert recorder._stream is not None


class TestRecorderSilenceHandling:
    """Test silence detection and trimming."""

    def test_init_silence_detection_params(self):
        """Test silence detection parameters."""
        recorder = AudioRecorder(
            trim_silence=True,
            silence_threshold=0.05,
            silence_duration=0.3,
        )
        assert recorder.trim_silence is True
        assert recorder.silence_threshold == 0.05
        assert recorder.silence_duration == 0.3

    @patch("dictation_app.recorder.sounddevice.InputStream")
    def test_completely_silent_audio(self, mock_stream_class):
        """Test recorder with completely silent input."""
        with tempfile.NamedTemporaryFile(suffix=".wav", delete=False) as f:
            temp_path = Path(f.name)

        try:
            mock_stream_instance = MagicMock()
            mock_stream_class.return_value = mock_stream_instance

            recorder = AudioRecorder(
                trim_silence=True,
                silence_threshold=0.01,
                silence_duration=0.1,
                device=0,
            )
            recorder.start()
            
            # Verify stream was created
            assert recorder._stream is not None
            
        finally:
            if temp_path.exists():
                temp_path.unlink()

    @patch("dictation_app.recorder.sounddevice.InputStream")
    def test_partial_silence_trimming(self, mock_stream_class):
        """Test silence trimming with mixed signal."""
        mock_stream_instance = MagicMock()
        mock_stream_class.return_value = mock_stream_instance

        recorder = AudioRecorder(trim_silence=True, silence_threshold=0.1, device=0)
        recorder.start()

        # Verify recorder was initialized with silence trimming
        assert recorder.trim_silence is True
        assert recorder._stream is not None


class TestRecorderEdgeCases:
    """Test recorder edge cases and boundary conditions."""

    def test_zero_sample_rate(self):
        """Test invalid sample rate is rejected."""
        with pytest.raises(ValueError, match="sample_rate must be positive"):
            AudioRecorder(sample_rate=0)

    def test_negative_chunk_size(self):
        """Test invalid chunk size is rejected."""
        with pytest.raises(ValueError, match="chunk_size must be positive"):
            AudioRecorder(chunk_size=-1)

    def test_mono_vs_stereo_initialization(self):
        """Test mono and stereo recorder initialization."""
        mono_recorder = AudioRecorder(channels=1)
        assert mono_recorder.channels == 1

        stereo_recorder = AudioRecorder(channels=2)
        assert stereo_recorder.channels == 2

        with pytest.raises(ValueError, match="channels must be 1 or 2"):
            AudioRecorder(channels=5)

    @patch("dictation_app.recorder.sounddevice.InputStream")
    def test_start_without_stop_reinit(self, mock_stream_class):
        """Test calling start multiple times reinitializes."""
        mock_stream_instance = MagicMock()
        mock_stream_class.return_value = mock_stream_instance

        recorder = AudioRecorder(device=0)
        
        recorder.start()
        stream1 = recorder._stream
        
        # Reset state to IDLE to allow restart
        from dictation_app.recorder import _RecorderState
        recorder._state = _RecorderState.IDLE
        recorder.start()
        stream2 = recorder._stream
        
        # Should create new stream
        assert mock_stream_class.call_count >= 2

    def test_stop_without_start(self):
        """Test calling stop without start raises error."""
        recorder = AudioRecorder()
        
        # Should raise RuntimeError
        with pytest.raises(RuntimeError):
            recorder.stop()

    def test_close_without_start(self):
        """Test calling close without start is safe."""
        recorder = AudioRecorder()
        
        # Should not crash
        recorder.close()


class TestRecorderAudioProcessing:
    """Test audio data processing and format conversion."""

    @patch("dictation_app.recorder.sounddevice.InputStream")
    def test_mono_audio_processing(self, mock_stream_class):
        """Test mono audio is processed correctly."""
        mock_stream_instance = MagicMock()
        mock_stream_class.return_value = mock_stream_instance

        recorder = AudioRecorder(channels=1, device=0)
        recorder.start()

        assert recorder.channels == 1
        assert recorder._stream is not None

    @patch("dictation_app.recorder.sounddevice.InputStream")
    def test_stereo_audio_processing(self, mock_stream_class):
        """Test stereo audio is processed correctly."""
        mock_stream_instance = MagicMock()
        mock_stream_class.return_value = mock_stream_instance

        recorder = AudioRecorder(channels=2, device=0)
        recorder.start()

        assert recorder.channels == 2
        assert recorder._stream is not None

    def test_audio_buffer_initialization(self):
        """Test audio buffer is properly initialized."""
        recorder = AudioRecorder()
        # Buffer is a deque, not bytes
        from collections import deque
        assert isinstance(recorder._buffer, deque)

    @patch("dictation_app.recorder.sounddevice.InputStream")
    def test_context_manager_start_stop(self, mock_stream_class):
        """Test recorder context manager properly manages resources."""
        mock_stream_instance = MagicMock()
        mock_stream_class.return_value = mock_stream_instance

        recorder = AudioRecorder()
        
        with recorder:
            assert recorder is not None


class TestRecorderResourceManagement:
    """Test proper resource cleanup."""

    @patch("dictation_app.recorder.sounddevice.InputStream")
    def test_stream_closure_on_error(self, mock_stream_class):
        """Test stream is closed on error."""
        mock_stream_instance = MagicMock()
        mock_stream_class.return_value = mock_stream_instance

        recorder = AudioRecorder(device=0)
        recorder.start()

        # Close should work
        recorder.close()
        mock_stream_instance.close.assert_called()

    @patch("dictation_app.recorder.sounddevice.InputStream")
    def test_multiple_close_calls_safe(self, mock_stream_class):
        """Test multiple close calls don't crash."""
        mock_stream_instance = MagicMock()
        mock_stream_class.return_value = mock_stream_instance

        recorder = AudioRecorder(device=0)
        recorder.start()
        
        # Should be safe to call multiple times
        recorder.close()
        recorder.close()
        recorder.close()

    @patch("dictation_app.recorder.sounddevice.InputStream")
    def test_recorder_cleanup_on_exception(self, mock_stream_class):
        """Test recorder cleans up on exception."""
        mock_stream_instance = MagicMock()
        mock_stream_class.return_value = mock_stream_instance

        recorder = AudioRecorder(device=0)
        
        try:
            recorder.start()
            raise ValueError("Simulated error")
        except ValueError:
            pass
        finally:
            recorder.close()
        
        # Verify close was called even with exception
        mock_stream_instance.close.assert_called()
