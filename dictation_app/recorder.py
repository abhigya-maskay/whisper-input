"""Audio capture and recording."""

import logging
from pathlib import Path

logger = logging.getLogger(__name__)


class AudioRecorder:
    """Manages audio capture via sounddevice.

    Streams PCM audio into a buffer with optional silence trimming and format conversion.
    """

    def __init__(
        self,
        sample_rate: int = 16000,
        channels: int = 1,
        chunk_size: int = 4096,
    ):
        """Initialize audio recorder.

        Args:
            sample_rate: Sample rate in Hz
            channels: Number of channels
            chunk_size: Chunk size for streaming
        """
        self.sample_rate = sample_rate
        self.channels = channels
        self.chunk_size = chunk_size
        self.is_recording = False
        logger.info(
            "AudioRecorder initialized: %d Hz, %d channels", sample_rate, channels
        )

    def start(self) -> None:
        """Start audio recording.

        TODO: Open sounddevice stream and begin buffering audio
        """
        self.is_recording = True
        logger.info("Audio recording started")

    def stop(self) -> Path:
        """Stop recording and return path to saved audio file.

        Returns:
            Path to temporary WAV file

        TODO: Close stream, trim silence, save to temporary file, return path
        """
        self.is_recording = False
        logger.info("Audio recording stopped")
        raise NotImplementedError("stop() not yet implemented")

    @staticmethod
    def list_devices() -> dict[int, str]:
        """List available audio capture devices.

        Returns:
            Dict mapping device IDs to descriptions

        TODO: Query sounddevice.query_devices() and return capture devices
        """
        raise NotImplementedError("list_devices() not yet implemented")
