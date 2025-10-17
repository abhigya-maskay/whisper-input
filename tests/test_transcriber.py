"""Tests for transcriber module."""

import pytest


class TestTranscriber:
    """Tests for Faster Whisper transcription."""

    @pytest.mark.skip(reason="Implementation pending")
    @pytest.mark.asyncio
    async def test_transcribe_async(self):
        """Test async transcription.

        TODO: Use prerecorded test WAV, verify transcription output
        """
        pass

    @pytest.mark.skip(reason="Implementation pending")
    @pytest.mark.asyncio
    async def test_model_lazy_loading(self):
        """Test lazy model loading on first transcription.

        TODO: Verify model loads only once
        """
        pass

    @pytest.mark.skip(reason="Implementation pending")
    @pytest.mark.asyncio
    async def test_transcription_timeout(self):
        """Test transcription timeout handling.

        TODO: Simulate long-running transcription, verify timeout
        """
        pass

    @pytest.mark.skip(reason="Implementation pending")
    def test_language_detection(self):
        """Test automatic language detection.

        TODO: Test with different language audio samples
        """
        pass
