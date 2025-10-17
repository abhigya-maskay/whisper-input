"""Tests for transcriber module."""

import tempfile
from pathlib import Path
from unittest.mock import AsyncMock, MagicMock, patch

import numpy as np
import pytest

from dictation_app.transcriber import Transcriber
from dictation_app._types import TranscriptionResult, TranscriptionSegment


class TestTranscriberInit:
    """Tests for Transcriber initialization."""

    def test_init_default_params(self):
        """Test transcriber initialization with defaults."""
        transcriber = Transcriber()
        assert transcriber.model_name == "base"
        assert transcriber.device == "cpu"
        assert transcriber.compute_type == "int8"
        assert transcriber.beam_size == 5
        assert transcriber.model_directory is None
        assert transcriber._model is None

    def test_init_custom_params(self):
        """Test transcriber initialization with custom parameters."""
        transcriber = Transcriber(
            model_name="small",
            device="cuda",
            compute_type="float16",
            model_directory="/custom/path",
            beam_size=10,
        )
        assert transcriber.model_name == "small"
        assert transcriber.device == "cuda"
        assert transcriber.compute_type == "float16"
        assert transcriber.model_directory == "/custom/path"
        assert transcriber.beam_size == 10

    def test_init_executor_ownership(self):
        """Test that transcriber owns executor if not provided."""
        from concurrent.futures import ThreadPoolExecutor

        transcriber1 = Transcriber()
        assert transcriber1._model_owned is True

        executor = ThreadPoolExecutor(max_workers=1)
        transcriber2 = Transcriber(executor=executor)
        assert transcriber2._model_owned is False
        executor.shutdown()


class TestTranscriberLazyLoading:
    """Tests for lazy model loading."""

    @pytest.mark.asyncio
    async def test_model_lazy_load_on_first_transcribe(self):
        """Test model loads only on first transcription."""
        with patch("faster_whisper.WhisperModel") as mock_model_class:
            mock_instance = MagicMock()
            mock_model_class.return_value = mock_instance
            
            # Create mock segment object
            mock_seg = MagicMock()
            mock_seg.text = "hello world"
            mock_seg.start = 0.0
            mock_seg.end = 1.0
            mock_seg.avg_logprob = 0.8
            
            # Create mock info object
            mock_info = MagicMock()
            mock_info.language = "en"
            
            mock_instance.transcribe.return_value = (
                iter([mock_seg]),  # generator
                mock_info,
            )

            transcriber = Transcriber()

            with tempfile.NamedTemporaryFile(suffix=".wav", delete=False) as tmp:
                tmp_path = Path(tmp.name)
                self._write_test_wav(tmp_path)

            try:
                with patch.object(transcriber, "_load_audio", return_value=np.zeros(16000)):
                    await transcriber.transcribe(tmp_path)
                    assert transcriber._model is not None
                    mock_model_class.assert_called_once()
            finally:
                tmp_path.unlink()
                await transcriber.shutdown()

    @pytest.mark.asyncio
    async def test_model_loads_only_once(self):
        """Test model reused on subsequent transcriptions."""
        with patch("faster_whisper.WhisperModel") as mock_model_class:
            mock_instance = MagicMock()
            mock_model_class.return_value = mock_instance
            
            # Create mock segment object
            mock_seg = MagicMock()
            mock_seg.text = "test"
            mock_seg.start = 0.0
            mock_seg.end = 1.0
            mock_seg.avg_logprob = 0.8
            
            # Create mock info object
            mock_info = MagicMock()
            mock_info.language = "en"
            
            mock_instance.transcribe.return_value = (
                iter([mock_seg]),  # generator
                mock_info,
            )

            transcriber = Transcriber()

            with tempfile.NamedTemporaryFile(suffix=".wav", delete=False) as tmp:
                tmp_path = Path(tmp.name)
                self._write_test_wav(tmp_path)

            try:
                with patch.object(transcriber, "_load_audio", return_value=np.zeros(16000)):
                    await transcriber.transcribe(tmp_path)
                    await transcriber.transcribe(tmp_path)
                    assert mock_model_class.call_count == 1
            finally:
                tmp_path.unlink()
                await transcriber.shutdown()

    @pytest.mark.asyncio
    async def test_model_load_failure(self):
        """Test error handling on model load failure."""
        with patch("faster_whisper.WhisperModel") as mock_model_class:
            mock_model_class.side_effect = RuntimeError("Model download failed")

            transcriber = Transcriber()

            with tempfile.NamedTemporaryFile(suffix=".wav", delete=False) as tmp:
                tmp_path = Path(tmp.name)
                self._write_test_wav(tmp_path)

            try:
                with pytest.raises(RuntimeError, match="Failed to load Whisper model"):
                    await transcriber.transcribe(tmp_path)
            finally:
                tmp_path.unlink()

    @staticmethod
    def _write_test_wav(path: Path, sample_rate: int = 16000, duration: float = 1.0):
        """Helper to write test WAV file."""
        import wave

        num_samples = int(sample_rate * duration)
        audio_data = np.sin(2 * np.pi * 440 * np.arange(num_samples) / sample_rate)
        audio_int16 = np.clip(audio_data * 32767, -32768, 32767).astype(np.int16)

        with wave.open(str(path), "wb") as wav_file:
            wav_file.setnchannels(1)
            wav_file.setsampwidth(2)
            wav_file.setframerate(sample_rate)
            wav_file.writeframes(audio_int16.tobytes())


class TestTranscriberExecution:
    """Tests for transcription execution."""

    @pytest.mark.asyncio
    async def test_transcribe_async_with_executor(self):
        """Test async transcription runs in executor."""
        with patch("faster_whisper.WhisperModel") as mock_model_class:
            mock_instance = MagicMock()
            mock_model_class.return_value = mock_instance
            
            # Create mock segment object
            mock_seg = MagicMock()
            mock_seg.text = "hello world"
            mock_seg.start = 0.0
            mock_seg.end = 1.0
            mock_seg.avg_logprob = 0.8
            
            # Create mock info object
            mock_info = MagicMock()
            mock_info.language = "en"
            
            mock_instance.transcribe.return_value = (
                iter([mock_seg]),  # generator
                mock_info,
            )

            transcriber = Transcriber()

            with tempfile.NamedTemporaryFile(suffix=".wav", delete=False) as tmp:
                tmp_path = Path(tmp.name)
                TestTranscriberLazyLoading._write_test_wav(tmp_path)

            try:
                with patch.object(transcriber, "_load_audio", return_value=np.zeros(16000)):
                    result = await transcriber.transcribe(tmp_path, language="en")
                    assert isinstance(result, TranscriptionResult)
                    assert result.text == "Hello world"
                    assert result.language == "en"
                    assert len(result.segments) > 0
            finally:
                tmp_path.unlink()
                await transcriber.shutdown()

    @pytest.mark.asyncio
    async def test_transcription_timeout(self):
        """Test transcription timeout handling."""
        with patch("faster_whisper.WhisperModel") as mock_model_class:
            mock_instance = MagicMock()
            mock_model_class.return_value = mock_instance

            def slow_transcribe(*args, **kwargs):
                import time

                time.sleep(2.0)
                return ("text", [], "en")

            mock_instance.transcribe.side_effect = slow_transcribe

            transcriber = Transcriber()

            with tempfile.NamedTemporaryFile(suffix=".wav", delete=False) as tmp:
                tmp_path = Path(tmp.name)
                TestTranscriberLazyLoading._write_test_wav(tmp_path)

            try:
                with patch.object(transcriber, "_load_audio", return_value=np.zeros(16000)):
                    with pytest.raises(RuntimeError, match="timed out"):
                        await transcriber.transcribe(tmp_path, timeout=0.1)
            finally:
                tmp_path.unlink()
                await transcriber.shutdown()

    @pytest.mark.asyncio
    async def test_transcribe_missing_audio_file(self):
        """Test error when audio file does not exist."""
        transcriber = Transcriber()

        with pytest.raises(RuntimeError, match="Audio file not found"):
            await transcriber.transcribe(Path("/nonexistent/audio.wav"))


class TestTextNormalization:
    """Tests for text normalization."""

    def test_normalize_strip_whitespace(self):
        """Test stripping leading/trailing whitespace."""
        transcriber = Transcriber()
        result = transcriber._normalize_text("  hello world  ")
        assert result == "Hello world"

    def test_normalize_remove_duplicate_spaces(self):
        """Test removing duplicate spaces."""
        transcriber = Transcriber()
        result = transcriber._normalize_text("hello    world")
        assert result == "Hello world"

    def test_normalize_remove_duplicate_newlines(self):
        """Test removing duplicate newlines."""
        transcriber = Transcriber()
        result = transcriber._normalize_text("hello\n\n\nworld")
        assert result == "Hello world"

    def test_normalize_fix_punctuation_spacing(self):
        """Test fixing spacing before punctuation."""
        transcriber = Transcriber()
        result = transcriber._normalize_text("hello , world !")
        assert result == "Hello, world!"

    def test_normalize_multiple_periods(self):
        """Test collapsing multiple periods."""
        transcriber = Transcriber()
        result = transcriber._normalize_text("wait....")
        assert result == "Wait."

    def test_normalize_capitalize_first_letter(self):
        """Test capitalizing first letter."""
        transcriber = Transcriber()
        result = transcriber._normalize_text("hello")
        assert result == "Hello"

    def test_normalize_empty_string(self):
        """Test normalizing empty string."""
        transcriber = Transcriber()
        result = transcriber._normalize_text("")
        assert result == ""

    def test_normalize_single_character(self):
        """Test normalizing single character."""
        transcriber = Transcriber()
        result = transcriber._normalize_text("a")
        assert result == "A"

    def test_normalize_already_capitalized(self):
        """Test normalizing already capitalized text."""
        transcriber = Transcriber()
        result = transcriber._normalize_text("Hello")
        assert result == "Hello"

    def test_normalize_complex_text(self):
        """Test normalizing complex text with multiple issues."""
        transcriber = Transcriber()
        result = transcriber._normalize_text("  hello   world .  this is  a test !!  ")
        expected = "Hello world. this is a test!!"
        assert result == expected


class TestAudioLoading:
    """Tests for audio loading and processing."""

    def test_load_audio_mono_16khz(self):
        """Test loading mono 16 kHz audio."""
        import sys
        transcriber = Transcriber()

        with tempfile.NamedTemporaryFile(suffix=".wav", delete=False) as tmp:
            tmp_path = Path(tmp.name)
            TestTranscriberLazyLoading._write_test_wav(tmp_path, sample_rate=16000)

        try:
            mock_soundfile = MagicMock()
            mock_soundfile.read.return_value = (
                np.sin(2 * np.pi * 440 * np.arange(16000) / 16000).astype(np.float32),
                16000
            )
            with patch.dict("sys.modules", {"soundfile": mock_soundfile}):
                audio = transcriber._load_audio(tmp_path)
                assert audio.dtype == np.float32
                assert audio.ndim == 1
                assert len(audio) == 16000
        finally:
            tmp_path.unlink()

    def test_load_audio_stereo_conversion(self):
        """Test stereo to mono conversion."""
        import sys
        transcriber = Transcriber()
        
        sample_rate = 16000
        duration = 1.0
        num_samples = int(sample_rate * duration)
        audio_left = np.sin(2 * np.pi * 440 * np.arange(num_samples) / sample_rate)
        audio_right = np.sin(2 * np.pi * 880 * np.arange(num_samples) / sample_rate)
        stereo_audio = np.column_stack((audio_left, audio_right)).astype(np.float32)

        with tempfile.NamedTemporaryFile(suffix=".wav", delete=False) as tmp:
            tmp_path = Path(tmp.name)

        try:
            mock_soundfile = MagicMock()
            mock_soundfile.read.return_value = (stereo_audio, sample_rate)
            with patch.dict("sys.modules", {"soundfile": mock_soundfile}):
                audio = transcriber._load_audio(tmp_path)
                assert audio.ndim == 1
                assert audio.dtype == np.float32
        finally:
            tmp_path.unlink()

    def test_load_audio_missing_file(self):
        """Test error handling for missing audio file."""
        transcriber = Transcriber()
        with pytest.raises(RuntimeError, match="Failed to load audio"):
            transcriber._load_audio(Path("/nonexistent/audio.wav"))


class TestTranscriberShutdown:
    """Tests for resource cleanup."""

    @pytest.mark.asyncio
    async def test_shutdown_clears_model(self):
        """Test shutdown clears model reference."""
        with patch("faster_whisper.WhisperModel") as mock_model_class:
            mock_instance = MagicMock()
            mock_model_class.return_value = mock_instance
            
            # Create mock segment object
            mock_seg = MagicMock()
            mock_seg.text = "test"
            mock_seg.start = 0.0
            mock_seg.end = 1.0
            mock_seg.avg_logprob = 0.8
            
            # Create mock info object
            mock_info = MagicMock()
            mock_info.language = "en"
            
            mock_instance.transcribe.return_value = (
                iter([mock_seg]),  # generator
                mock_info,
            )

            transcriber = Transcriber()

            with tempfile.NamedTemporaryFile(suffix=".wav", delete=False) as tmp:
                tmp_path = Path(tmp.name)
                TestTranscriberLazyLoading._write_test_wav(tmp_path)

            try:
                with patch.object(transcriber, "_load_audio", return_value=np.zeros(16000)):
                    await transcriber.transcribe(tmp_path)
                    assert transcriber._model is not None
                    await transcriber.shutdown()
                    assert transcriber._model is None
            finally:
                tmp_path.unlink()

    @pytest.mark.asyncio
    async def test_shutdown_executor(self):
        """Test shutdown calls executor.shutdown()."""
        from concurrent.futures import ThreadPoolExecutor

        with patch("faster_whisper.WhisperModel"):
            transcriber = Transcriber()
            with patch.object(transcriber.executor, "shutdown") as mock_shutdown:
                await transcriber.shutdown()
                mock_shutdown.assert_called_once_with(wait=True)
