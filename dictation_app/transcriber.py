"""Audio transcription via Faster Whisper."""

import logging
from concurrent.futures import ThreadPoolExecutor
from pathlib import Path

from dictation_app._types import TranscriptionResult

logger = logging.getLogger(__name__)


class Transcriber:
    """Encapsulates Faster Whisper model and transcription logic.

    Runs transcription inside a thread pool executor to avoid blocking the event loop.
    """

    def __init__(
        self,
        model_name: str = "base",
        device: str = "cpu",
        compute_type: str = "int8",
        executor: ThreadPoolExecutor | None = None,
    ):
        """Initialize transcriber.

        Args:
            model_name: Faster Whisper model name (tiny, base, small, etc.)
            device: Device to run on (cpu, cuda, auto)
            compute_type: Compute precision (int8, float16, float32)
            executor: Optional ThreadPoolExecutor for transcription tasks
        """
        self.model_name = model_name
        self.device = device
        self.compute_type = compute_type
        self.executor = executor or ThreadPoolExecutor(max_workers=1)
        self.model = None  # Loaded lazily on first transcription
        logger.info(
            "Transcriber initialized: model=%s, device=%s, compute_type=%s",
            model_name,
            device,
            compute_type,
        )

    async def transcribe(
        self,
        audio_path: Path,
        language: str = "auto",
        timeout: float = 30.0,
    ) -> TranscriptionResult:
        """Transcribe audio file asynchronously.

        Args:
            audio_path: Path to WAV file
            language: Language code (auto for detection)
            timeout: Maximum time in seconds

        Returns:
            TranscriptionResult with text and metadata

        TODO: Load model lazily, run transcription in executor, apply post-processing
        """
        logger.info("Starting transcription of %s", audio_path)
        raise NotImplementedError("transcribe() not yet implemented")
