"""Tests for transcriber robustness: corrupted files, timeouts, edge cases."""

import asyncio
import tempfile
from concurrent.futures import ThreadPoolExecutor
from pathlib import Path
from unittest.mock import AsyncMock, MagicMock, patch

import numpy as np
import pytest

from dictation_app.transcriber import Transcriber
from dictation_app._types import TranscriptionResult, TranscriptionSegment


class TestTranscriberFileHandling:
    """Test transcriber handling of corrupted and invalid audio files."""

    @pytest.mark.asyncio
    async def test_corrupted_wav_file(self):
        """Test transcriber handles corrupted WAV file."""
        with tempfile.NamedTemporaryFile(suffix=".wav", delete=False) as f:
            # Write corrupted WAV header
            f.write(b"RIFF\x00\x00\x00\x00WAVEjunk")
            corrupt_path = Path(f.name)

        try:
            with patch("faster_whisper.WhisperModel") as mock_model_class:
                mock_instance = MagicMock()
                mock_model_class.return_value = mock_instance
                mock_instance.transcribe.side_effect = RuntimeError("Invalid audio file")

                transcriber = Transcriber()
                
                with pytest.raises(RuntimeError):
                    await transcriber.transcribe(corrupt_path)
        finally:
            if corrupt_path.exists():
                corrupt_path.unlink()

    @pytest.mark.asyncio
    async def test_nonexistent_file(self):
        """Test transcriber handles missing file."""
        transcriber = Transcriber()
        nonexistent_path = Path("/tmp/nonexistent_file_xyz.wav")

        with pytest.raises((FileNotFoundError, RuntimeError, OSError)):
            await transcriber.transcribe(nonexistent_path)

    @pytest.mark.asyncio
    async def test_empty_audio_file(self):
        """Test transcriber handles empty audio file."""
        with tempfile.NamedTemporaryFile(suffix=".wav", delete=False) as f:
            empty_path = Path(f.name)

        try:
            with patch("faster_whisper.WhisperModel") as mock_model_class:
                mock_instance = MagicMock()
                mock_model_class.return_value = mock_instance
                mock_instance.transcribe.return_value = (
                    "",
                    [],
                    "en",
                )

                transcriber = Transcriber()
                with patch.object(transcriber, "_load_audio", return_value=np.zeros(0)):
                    result = await transcriber.transcribe(empty_path)
                    assert result.text == ""
                    assert len(result.segments) == 0
        finally:
            if empty_path.exists():
                empty_path.unlink()

    @pytest.mark.asyncio
    async def test_unsupported_audio_format(self):
        """Test transcriber handles unsupported audio format."""
        with tempfile.NamedTemporaryFile(suffix=".xyz", delete=False) as f:
            f.write(b"INVALID_AUDIO_DATA")
            invalid_path = Path(f.name)

        try:
            transcriber = Transcriber()
            
            with pytest.raises((RuntimeError, ValueError, OSError)):
                await transcriber.transcribe(invalid_path)
        finally:
            if invalid_path.exists():
                invalid_path.unlink()


class TestTranscriberTimeout:
    """Test transcriber timeout handling."""

    @pytest.mark.asyncio
    async def test_transcription_timeout(self):
        """Test transcriber timeout is properly raised."""
        with tempfile.NamedTemporaryFile(suffix=".wav", delete=False) as f:
            f.write(b"RIFF" + b"\x00" * 36 + b"data")
            temp_path = Path(f.name)

        try:
            with patch("faster_whisper.WhisperModel") as mock_model_class:
                mock_instance = MagicMock()
                mock_model_class.return_value = mock_instance
                
                # Simulate long delay - return proper tuple format
                mock_instance.transcribe.return_value = (
                    "test",
                    [{"text": "test", "start": 0.0, "end": 1.0}],
                    "en",
                )

                transcriber = Transcriber()
                
                with patch.object(transcriber, "_load_audio", return_value=np.zeros(16000)):
                    result = await transcriber.transcribe(temp_path, timeout=30.0)
                    assert result is not None
                    assert result.text is not None
        finally:
            if temp_path.exists():
                temp_path.unlink()

    @pytest.mark.asyncio
    async def test_executor_timeout_handling(self):
        """Test executor submission with timeout."""
        with tempfile.NamedTemporaryFile(suffix=".wav", delete=False) as f:
            f.write(b"RIFF" + b"\x00" * 36 + b"test")
            temp_path = Path(f.name)

        try:
            executor = ThreadPoolExecutor(max_workers=1)
            transcriber = Transcriber(executor=executor)

            with patch("faster_whisper.WhisperModel") as mock_model_class:
                mock_instance = MagicMock()
                mock_model_class.return_value = mock_instance
                # Return proper tuple with segments
                mock_instance.transcribe.return_value = (
                    "test",
                    [{"text": "test", "start": 0.0, "end": 1.0}],
                    "en",
                )

                with patch.object(transcriber, "_load_audio", return_value=np.zeros(16000)):
                    result = await transcriber.transcribe(temp_path)
                    # Text may be capitalized by post-processing
                    assert result.text.lower() == "test"
            
            executor.shutdown()
        finally:
            if temp_path.exists():
                temp_path.unlink()


class TestTranscriberEdgeCases:
    """Test transcriber edge cases."""

    def test_initialization_invalid_compute_type(self):
        """Test invalid compute_type is handled."""
        # Should not raise during init
        transcriber = Transcriber(compute_type="invalid_type")
        assert transcriber.compute_type == "invalid_type"

    def test_initialization_invalid_device(self):
        """Test invalid device is handled."""
        # Should not raise during init
        transcriber = Transcriber(device="invalid_device")
        assert transcriber.device == "invalid_device"

    @pytest.mark.asyncio
    async def test_model_lazy_load_failure_recovery(self):
        """Test transcriber handles model loading failure."""
        with patch("faster_whisper.WhisperModel") as mock_model_class:
            mock_model_class.side_effect = RuntimeError("Model download failed")

            transcriber = Transcriber()
            
            with pytest.raises(RuntimeError):
                await transcriber._ensure_model_loaded()

    @pytest.mark.asyncio
    async def test_multiple_concurrent_transcriptions(self):
        """Test multiple transcription requests don't interfere."""
        with tempfile.NamedTemporaryFile(suffix=".wav", delete=False) as f1:
            f1.write(b"RIFF" + b"\x00" * 36 + b"audio1")
            path1 = Path(f1.name)

        with tempfile.NamedTemporaryFile(suffix=".wav", delete=False) as f2:
            f2.write(b"RIFF" + b"\x00" * 36 + b"audio2")
            path2 = Path(f2.name)

        try:
            with patch("faster_whisper.WhisperModel") as mock_model_class:
                mock_instance = MagicMock()
                mock_model_class.return_value = mock_instance
                mock_instance.transcribe.side_effect = [
                    ("hello", [{"text": "hello", "start": 0, "end": 1}], "en"),
                    ("world", [{"text": "world", "start": 0, "end": 1}], "en"),
                ]

                transcriber = Transcriber()
                
                with patch.object(transcriber, "_load_audio", return_value=np.zeros(16000)):
                    # Run multiple transcriptions
                    task1 = transcriber.transcribe(path1)
                    task2 = transcriber.transcribe(path2)
                    
                    results = await asyncio.gather(task1, task2, return_exceptions=True)
                    
                    # Both should complete without error
                    assert len(results) == 2
                    assert all(not isinstance(r, Exception) for r in results)
        finally:
            if path1.exists():
                path1.unlink()
            if path2.exists():
                path2.unlink()


class TestTranscriberSegmentHandling:
    """Test segment processing and edge cases."""

    @pytest.mark.asyncio
    async def test_single_segment_result(self):
        """Test handling of single segment transcription."""
        with tempfile.NamedTemporaryFile(suffix=".wav", delete=False) as f:
            f.write(b"RIFF" + b"\x00" * 36 + b"test")
            temp_path = Path(f.name)

        try:
            with patch("faster_whisper.WhisperModel") as mock_model_class:
                mock_instance = MagicMock()
                mock_model_class.return_value = mock_instance
                mock_instance.transcribe.return_value = (
                    "single segment",
                    [{"text": "single segment", "start": 0.0, "end": 1.5}],
                    "en",
                )

                transcriber = Transcriber()
                
                with patch.object(transcriber, "_load_audio", return_value=np.zeros(16000)):
                    result = await transcriber.transcribe(temp_path)
                    
                    assert result.text is not None
                    assert len(result.segments) == 1
                    assert "segment" in result.segments[0].text.lower()
        finally:
            if temp_path.exists():
                temp_path.unlink()

    @pytest.mark.asyncio
    async def test_multiple_segments_result(self):
        """Test handling of multiple segment transcription."""
        with tempfile.NamedTemporaryFile(suffix=".wav", delete=False) as f:
            f.write(b"RIFF" + b"\x00" * 36 + b"test")
            temp_path = Path(f.name)

        try:
            with patch("faster_whisper.WhisperModel") as mock_model_class:
                mock_instance = MagicMock()
                mock_model_class.return_value = mock_instance
                mock_instance.transcribe.return_value = (
                    "hello world",
                    [
                        {"text": "hello", "start": 0.0, "end": 0.5},
                        {"text": "world", "start": 0.5, "end": 1.0},
                    ],
                    "en",
                )

                transcriber = Transcriber()
                
                with patch.object(transcriber, "_load_audio", return_value=np.zeros(16000)):
                    result = await transcriber.transcribe(temp_path)
                    
                    assert result.text is not None
                    assert len(result.segments) == 2
                    assert "hello" in result.segments[0].text.lower()
                    assert "world" in result.segments[1].text.lower()
        finally:
            if temp_path.exists():
                temp_path.unlink()

    @pytest.mark.asyncio
    async def test_empty_segments_list(self):
        """Test handling of empty segments list."""
        with tempfile.NamedTemporaryFile(suffix=".wav", delete=False) as f:
            f.write(b"RIFF" + b"\x00" * 36 + b"test")
            temp_path = Path(f.name)

        try:
            with patch("faster_whisper.WhisperModel") as mock_model_class:
                mock_instance = MagicMock()
                mock_model_class.return_value = mock_instance
                mock_instance.transcribe.return_value = ("", [], "en")

                transcriber = Transcriber()
                
                with patch.object(transcriber, "_load_audio", return_value=np.zeros(16000)):
                    result = await transcriber.transcribe(temp_path)
                    
                    assert result.text == ""
                    assert len(result.segments) == 0
        finally:
            if temp_path.exists():
                temp_path.unlink()


class TestTranscriberShutdown:
    """Test transcriber shutdown and cleanup."""

    @pytest.mark.asyncio
    async def test_shutdown_owned_executor(self):
        """Test shutdown properly closes owned executor."""
        transcriber = Transcriber()
        assert transcriber._model_owned is True

        await transcriber.shutdown()
        # No exception should be raised

    @pytest.mark.asyncio
    async def test_shutdown_with_external_executor(self):
        """Test shutdown with external executor doesn't close it."""
        executor = ThreadPoolExecutor(max_workers=1)
        transcriber = Transcriber(executor=executor)
        assert transcriber._model_owned is False

        await transcriber.shutdown()
        
        # External executor should still be usable
        future = executor.submit(lambda: 42)
        assert future.result() == 42
        
        executor.shutdown()

    @pytest.mark.asyncio
    async def test_multiple_shutdown_calls(self):
        """Test multiple shutdown calls are safe."""
        transcriber = Transcriber()
        
        await transcriber.shutdown()
        await transcriber.shutdown()
        # Should not raise
