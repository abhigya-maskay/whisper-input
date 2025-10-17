"""Tests for audio recorder module."""

import tempfile
from pathlib import Path
from unittest.mock import MagicMock, patch

import numpy as np
import pytest

from dictation_app.recorder import AudioRecorder


class TestAudioRecorderInit:
    """Tests for AudioRecorder initialization."""

    def test_init_default_params(self):
        """Test recorder initialization with default parameters."""
        recorder = AudioRecorder()
        assert recorder.sample_rate == 16000
        assert recorder.channels == 1
        assert recorder.chunk_size == 4096
        assert recorder.trim_silence is False

    def test_init_custom_params(self):
        """Test recorder initialization with custom parameters."""
        recorder = AudioRecorder(
            sample_rate=44100,
            channels=2,
            chunk_size=2048,
            trim_silence=True,
            silence_threshold=0.05,
            silence_duration=0.5,
        )
        assert recorder.sample_rate == 44100
        assert recorder.channels == 2
        assert recorder.chunk_size == 2048
        assert recorder.trim_silence is True
        assert recorder.silence_threshold == 0.05
        assert recorder.silence_duration == 0.5

    def test_init_invalid_sample_rate(self):
        """Test initialization fails with invalid sample rate."""
        with pytest.raises(ValueError, match="sample_rate must be positive"):
            AudioRecorder(sample_rate=0)
        with pytest.raises(ValueError, match="sample_rate must be positive"):
            AudioRecorder(sample_rate=-1)

    def test_init_invalid_channels(self):
        """Test initialization fails with invalid channel count."""
        with pytest.raises(ValueError, match="channels must be 1 or 2"):
            AudioRecorder(channels=0)
        with pytest.raises(ValueError, match="channels must be 1 or 2"):
            AudioRecorder(channels=3)

    def test_init_invalid_chunk_size(self):
        """Test initialization fails with invalid chunk size."""
        with pytest.raises(ValueError, match="chunk_size must be positive"):
            AudioRecorder(chunk_size=0)


class TestAudioRecorderContextManager:
    """Tests for context manager behavior."""

    @patch("dictation_app.recorder.sounddevice.InputStream")
    def test_context_manager_cleanup(self, mock_input_stream):
        """Test context manager ensures cleanup."""
        mock_stream_instance = MagicMock()
        mock_input_stream.return_value = mock_stream_instance

        with AudioRecorder() as recorder:
            recorder.start()
            assert recorder._stream is not None

        mock_stream_instance.stop.assert_called_once()
        mock_stream_instance.close.assert_called_once()
        assert recorder._stream is None


class TestAudioRecorderStartStop:
    """Tests for recorder start/stop lifecycle."""

    @patch("dictation_app.recorder.sounddevice.InputStream")
    def test_start_creates_stream(self, mock_input_stream):
        """Test start() creates and starts the input stream."""
        mock_stream_instance = MagicMock()
        mock_input_stream.return_value = mock_stream_instance

        recorder = AudioRecorder(sample_rate=16000, channels=1)
        recorder.start()

        mock_input_stream.assert_called_once()
        call_kwargs = mock_input_stream.call_args[1]
        assert call_kwargs["samplerate"] == 16000
        assert call_kwargs["channels"] == 1
        assert call_kwargs["dtype"] == "float32"
        mock_stream_instance.start.assert_called_once()

    @patch("dictation_app.recorder.sounddevice.InputStream")
    def test_start_registers_callback(self, mock_input_stream):
        """Test start() registers the callback function."""
        mock_stream_instance = MagicMock()
        mock_input_stream.return_value = mock_stream_instance

        recorder = AudioRecorder()
        recorder.start()

        call_kwargs = mock_input_stream.call_args[1]
        assert call_kwargs["callback"] == recorder._callback

    @patch("dictation_app.recorder.sounddevice.InputStream")
    def test_start_twice_raises_error(self, mock_input_stream):
        """Test calling start() twice raises RuntimeError."""
        mock_stream_instance = MagicMock()
        mock_input_stream.return_value = mock_stream_instance

        recorder = AudioRecorder()
        recorder.start()

        with pytest.raises(RuntimeError, match="Cannot start recording"):
            recorder.start()

    @patch("dictation_app.recorder.sounddevice.InputStream")
    def test_start_stream_error(self, mock_input_stream):
        """Test start() handles stream creation errors."""
        mock_input_stream.side_effect = RuntimeError("Device busy")

        recorder = AudioRecorder()
        with pytest.raises(RuntimeError, match="Failed to start audio stream"):
            recorder.start()

    @patch("dictation_app.recorder.sounddevice.InputStream")
    def test_stop_without_start_raises_error(self, mock_input_stream):
        """Test calling stop() without start() raises RuntimeError."""
        recorder = AudioRecorder()
        with pytest.raises(RuntimeError, match="recorder not recording"):
            recorder.stop()

    @patch("dictation_app.recorder.sounddevice.InputStream")
    def test_stop_without_audio_raises_error(self, mock_input_stream):
        """Test stop() raises error if no audio frames captured."""
        mock_stream_instance = MagicMock()
        mock_input_stream.return_value = mock_stream_instance

        recorder = AudioRecorder()
        recorder.start()

        with pytest.raises(RuntimeError, match="No audio frames captured"):
            recorder.stop()

    @patch("dictation_app.recorder.sounddevice.InputStream")
    def test_stop_successful_recording(self, mock_input_stream):
        """Test successful stop() returns temporary file path."""
        mock_stream_instance = MagicMock()
        mock_input_stream.return_value = mock_stream_instance

        recorder = AudioRecorder(sample_rate=16000, channels=1)
        recorder.start()

        # Simulate audio data
        audio_data = np.random.randn(16000).astype(np.float32)
        recorder._buffer.append(audio_data)

        result = recorder.stop()

        assert isinstance(result, Path)
        assert result.exists()
        assert result.suffix == ".wav"

        result.unlink()

    @patch("dictation_app.recorder.sounddevice.InputStream")
    def test_stop_closes_stream(self, mock_input_stream):
        """Test stop() properly closes the stream."""
        mock_stream_instance = MagicMock()
        mock_input_stream.return_value = mock_stream_instance

        recorder = AudioRecorder()
        recorder.start()
        recorder._buffer.append(np.random.randn(4096).astype(np.float32))

        recorder.stop()

        mock_stream_instance.stop.assert_called_once()
        mock_stream_instance.close.assert_called_once()


class TestSilenceTrimming:
    """Tests for silence trimming functionality."""

    def test_trim_silence_with_silence_regions(self):
        """Test silence trimming removes leading/trailing silence."""
        recorder = AudioRecorder(
            sample_rate=16000,
            channels=1,
            trim_silence=True,
            silence_threshold=0.02,
            silence_duration=0.1,
        )

        # Create audio with distinct silence regions: [quiet] + [loud] + [quiet]
        quiet = np.random.randn(3200).astype(np.float32) * 0.005
        loud = np.random.randn(8000).astype(np.float32) * 0.8
        frames = np.concatenate([quiet, loud, quiet])

        trimmed = recorder._trim_silence(frames)

        # Should trim some of the quiet regions
        assert len(trimmed) < len(frames)

    def test_trim_silence_preserves_loud_signal(self):
        """Test silence trimming preserves loud signal regions."""
        recorder = AudioRecorder(
            sample_rate=16000,
            channels=1,
            trim_silence=True,
            silence_threshold=0.02,
            silence_duration=0.1,
        )

        loud_signal = np.random.randn(16000).astype(np.float32) * 0.8
        trimmed = recorder._trim_silence(loud_signal)

        # Loud signal should be preserved (less than 10% trimmed)
        assert len(trimmed) > len(loud_signal) * 0.5

    def test_trim_silence_short_audio(self):
        """Test silence trimming on audio shorter than min duration."""
        recorder = AudioRecorder(
            sample_rate=16000,
            channels=1,
            trim_silence=True,
            silence_duration=1.0,
        )

        short_audio = np.random.randn(100).astype(np.float32)
        trimmed = recorder._trim_silence(short_audio)

        assert np.array_equal(trimmed, short_audio)

    def test_compute_rms(self):
        """Test RMS computation per chunk."""
        recorder = AudioRecorder(sample_rate=16000)

        signal = np.sin(np.linspace(0, 2 * np.pi, 1600)).astype(np.float32)
        rms_values = recorder._compute_rms(signal)

        assert len(rms_values) > 0
        assert np.all(rms_values >= 0)

    @patch("dictation_app.recorder.sounddevice.InputStream")
    def test_stop_with_silence_trimming_enabled(self, mock_input_stream):
        """Test stop() applies silence trimming when enabled."""
        mock_stream_instance = MagicMock()
        mock_input_stream.return_value = mock_stream_instance

        recorder = AudioRecorder(sample_rate=16000, trim_silence=True)
        recorder.start()

        # Add audio with silence regions
        silence = np.zeros(1600, dtype=np.float32)
        signal = np.random.randn(8000).astype(np.float32) * 0.5
        frames = np.concatenate([silence, signal, silence])
        recorder._buffer.append(frames)

        result = recorder.stop()

        assert isinstance(result, Path)
        assert result.exists()
        result.unlink()


class TestWAVFileCreation:
    """Tests for WAV file creation and format."""

    @patch("dictation_app.recorder.sounddevice.InputStream")
    def test_wav_file_created_with_correct_metadata(self, mock_input_stream):
        """Test WAV file has correct metadata."""
        mock_stream_instance = MagicMock()
        mock_input_stream.return_value = mock_stream_instance

        recorder = AudioRecorder(sample_rate=44100, channels=2)
        recorder.start()
        recorder._buffer.append(np.random.randn(44100, 2).astype(np.float32))

        result = recorder.stop()

        import wave

        with wave.open(str(result), "rb") as wav_file:
            assert wav_file.getframerate() == 44100
            assert wav_file.getnchannels() == 2
            assert wav_file.getsampwidth() == 2

        result.unlink()

    @patch("dictation_app.recorder.sounddevice.InputStream")
    def test_wav_file_cleanup_on_error(self, mock_input_stream):
        """Test temporary file cleanup on exception."""
        mock_stream_instance = MagicMock()
        mock_input_stream.return_value = mock_stream_instance

        recorder = AudioRecorder()
        recorder.start()

        with pytest.raises(RuntimeError):
            recorder.stop()

    def test_write_wav_clips_audio(self):
        """Test audio clipping in WAV write."""
        recorder = AudioRecorder()

        audio = np.array([0, 0.5, 1.0, -1.0, 2.0], dtype=np.float32)

        with tempfile.NamedTemporaryFile(suffix=".wav", delete=False) as tmp:
            tmp_path = Path(tmp.name)

        recorder._write_wav(tmp_path, audio)

        import wave

        with wave.open(str(tmp_path), "rb") as wav_file:
            data = wav_file.readframes(wav_file.getnframes())

        tmp_path.unlink()
        assert len(data) > 0


class TestDeviceDiscovery:
    """Tests for audio device discovery."""

    @patch("dictation_app.recorder.sounddevice.query_devices")
    def test_list_devices_returns_input_devices(self, mock_query):
        """Test list_devices filters for input devices."""
        mock_query.return_value = [
            {"name": "Microphone", "max_input_channels": 2, "max_output_channels": 0},
            {"name": "Speakers", "max_input_channels": 0, "max_output_channels": 2},
            {"name": "USB Audio", "max_input_channels": 1, "max_output_channels": 1},
        ]

        devices = AudioRecorder.list_devices()

        assert len(devices) == 2
        assert 0 in devices
        assert 2 in devices
        assert devices[0] == "Microphone"
        assert devices[2] == "USB Audio"

    @patch("dictation_app.recorder.sounddevice.query_devices")
    def test_list_devices_single_device(self, mock_query):
        """Test list_devices with single device response."""
        mock_query.return_value = {
            "name": "Microphone",
            "max_input_channels": 2,
            "max_output_channels": 0,
        }

        devices = AudioRecorder.list_devices()

        assert len(devices) == 1
        assert 0 in devices

    @patch("dictation_app.recorder.sounddevice.query_devices")
    def test_list_devices_no_input_devices(self, mock_query):
        """Test list_devices with no input devices."""
        mock_query.return_value = [
            {"name": "Speakers", "max_input_channels": 0, "max_output_channels": 2},
        ]

        devices = AudioRecorder.list_devices()

        assert len(devices) == 0

    @patch("dictation_app.recorder.sounddevice.query_devices")
    def test_list_devices_portaudio_error(self, mock_query):
        """Test list_devices handles PortAudioError."""
        import sounddevice

        mock_query.side_effect = sounddevice.PortAudioError("Device error")

        devices = AudioRecorder.list_devices()

        assert devices == {}

    @patch("dictation_app.recorder.sounddevice.query_devices")
    def test_list_devices_generic_error(self, mock_query):
        """Test list_devices handles generic errors."""
        mock_query.side_effect = RuntimeError("Unexpected error")

        devices = AudioRecorder.list_devices()

        assert devices == {}


class TestAudioCallback:
    """Tests for the audio stream callback."""

    def test_callback_appends_data_to_buffer(self):
        """Test callback appends audio data to buffer."""
        recorder = AudioRecorder()

        indata = np.random.randn(4096).astype(np.float32)
        recorder._callback(indata, 4096, None, None)

        assert len(recorder._buffer) == 1
        assert np.array_equal(recorder._buffer[0], indata)

    def test_callback_copies_data(self):
        """Test callback copies data (doesn't reference original)."""
        recorder = AudioRecorder()

        indata = np.array([1.0, 2.0, 3.0], dtype=np.float32)
        recorder._callback(indata, 3, None, None)

        indata[0] = 999.0
        assert recorder._buffer[0][0] != 999.0

    def test_callback_handles_status_flags(self):
        """Test callback logs status flags."""
        recorder = AudioRecorder()

        indata = np.random.randn(4096).astype(np.float32)
        import sounddevice

        with patch("dictation_app.recorder.logger") as mock_logger:
            recorder._callback(indata, 4096, None, sounddevice.CallbackFlags())
            mock_logger.warning.assert_not_called()


class TestClose:
    """Tests for explicit close method."""

    @patch("dictation_app.recorder.sounddevice.InputStream")
    def test_close_closes_stream(self, mock_input_stream):
        """Test close() properly closes the stream."""
        mock_stream_instance = MagicMock()
        mock_input_stream.return_value = mock_stream_instance

        recorder = AudioRecorder()
        recorder.start()
        recorder.close()

        mock_stream_instance.stop.assert_called_once()
        mock_stream_instance.close.assert_called_once()

    @patch("dictation_app.recorder.sounddevice.InputStream")
    def test_close_handles_errors(self, mock_input_stream):
        """Test close() handles errors gracefully."""
        mock_stream_instance = MagicMock()
        mock_stream_instance.stop.side_effect = RuntimeError("Error")
        mock_input_stream.return_value = mock_stream_instance

        recorder = AudioRecorder()
        recorder.start()
        recorder.close()

        assert recorder._stream is None
