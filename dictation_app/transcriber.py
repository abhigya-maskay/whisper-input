"""Audio transcription via Faster Whisper."""

import asyncio
import logging
import re
import time
from concurrent.futures import ThreadPoolExecutor
from pathlib import Path

import numpy as np

from dictation_app._types import TranscriptionResult, TranscriptionSegment

logger = logging.getLogger(__name__)


class Transcriber:
    """Encapsulates Faster Whisper model and transcription logic.

    Runs transcription inside a thread pool executor to avoid blocking the event loop.
    Lazy-loads model on first transcription to avoid startup overhead.
    """

    def __init__(
        self,
        model_name: str = "base",
        device: str = "cpu",
        compute_type: str = "int8",
        model_directory: str | None = None,
        beam_size: int = 5,
        executor: ThreadPoolExecutor | None = None,
    ):
        """Initialize transcriber.

        Args:
            model_name: Faster Whisper model name (tiny, base, small, etc.)
            device: Device to run on (cpu, cuda, auto)
            compute_type: Compute precision (int8, float16, float32)
            model_directory: Custom cache directory for model weights
            beam_size: Beam search width for decoding
            executor: Optional ThreadPoolExecutor for transcription tasks
        """
        self.model_name = model_name
        self.device = device
        self.compute_type = compute_type
        self.model_directory = model_directory
        self.beam_size = beam_size
        self.executor = executor or ThreadPoolExecutor(max_workers=1)
        self._model_owned = executor is None
        self._model = None
        self._model_lock = asyncio.Lock()
        logger.info(
            "Transcriber initialized: model=%s, device=%s, compute_type=%s, beam_size=%d",
            model_name,
            device,
            compute_type,
            beam_size,
        )

    async def _ensure_model_loaded(self) -> None:
        """Lazy-load WhisperModel on first use.

        Uses asyncio.Lock to prevent concurrent load attempts.
        Logs initialization duration and device/compute type info.

        Raises:
            RuntimeError: If model fails to load
        """
        async with self._model_lock:
            if self._model is not None:
                return

            logger.info(
                "Loading Faster Whisper model: %s (device=%s, compute_type=%s)",
                self.model_name,
                self.device,
                self.compute_type,
            )

            try:
                from faster_whisper import WhisperModel

                start_time = time.perf_counter()
                self._model = WhisperModel(
                    self.model_name,
                    device=self.device,
                    compute_type=self.compute_type,
                    download_root=self.model_directory,
                )
                duration = time.perf_counter() - start_time
                logger.info(
                    "Model loaded successfully in %.2f seconds", duration
                )
            except Exception as e:
                logger.error(
                    "Failed to load model %s on device %s: %s",
                    self.model_name,
                    self.device,
                    e,
                )
                raise RuntimeError(
                    f"Failed to load Whisper model '{self.model_name}' on device "
                    f"'{self.device}' with compute_type '{self.compute_type}': {e}"
                ) from e

    async def transcribe(
        self,
        audio_path: Path,
        language: str = "en",
        timeout: float = 30.0,
    ) -> TranscriptionResult:
        """Transcribe audio file asynchronously.

        Loads model lazily on first call, runs transcription in thread pool
        to avoid blocking the event loop, and applies text post-processing.

        Args:
            audio_path: Path to WAV file
            language: Language code (e.g., "en", "fr") or "auto" for detection
            timeout: Maximum time in seconds

        Returns:
            TranscriptionResult with text, language, and segments

        Raises:
            RuntimeError: If model loading or transcription fails
            asyncio.TimeoutError: If transcription exceeds timeout
        """
        audio_path = Path(audio_path)
        if not audio_path.exists():
            raise RuntimeError(f"Audio file not found: {audio_path}")

        await self._ensure_model_loaded()

        logger.info("Starting transcription of %s (language=%s)", audio_path, language)

        try:
            result = await asyncio.wait_for(
                asyncio.get_event_loop().run_in_executor(
                    self.executor,
                    self._transcribe_sync,
                    audio_path,
                    language,
                ),
                timeout=timeout,
            )
            logger.info("Transcription completed: %d segments", len(result.segments))
            return result
        except asyncio.TimeoutError as e:
            logger.error("Transcription timed out after %.1f seconds", timeout)
            raise RuntimeError(
                f"Transcription timed out after {timeout} seconds"
            ) from e
        except RuntimeError:
            raise
        except Exception as e:
            logger.error("Transcription failed: %s", e, exc_info=True)
            raise RuntimeError(f"Transcription failed: {e}") from e

    def _transcribe_sync(
        self,
        audio_path: Path,
        language: str,
    ) -> TranscriptionResult:
        """Synchronous transcription (runs in thread pool).

        Args:
            audio_path: Path to audio file
            language: Language code

        Returns:
            TranscriptionResult with text and segments

        Raises:
            RuntimeError: If transcription fails
        """
        try:
            if self._model is None:
                raise RuntimeError("Model not loaded")

            audio_data = self._load_audio(audio_path)

            result = self._model.transcribe(
                audio_data,
                language=language if language != "auto" else None,
                beam_size=self.beam_size,
            )

            segments = [
                TranscriptionSegment(
                    text=seg["text"],
                    start=seg["start"],
                    end=seg["end"],
                    confidence=seg.get("confidence", 0.0),
                )
                for seg in result[1]
            ]

            primary_text = result[0]
            detected_language = result[2] if len(result) > 2 else language

            primary_text = self._normalize_text(primary_text)

            return TranscriptionResult(
                text=primary_text,
                language=detected_language,
                confidence=0.0,
                segments=segments,
            )

        except Exception as e:
            logger.error("Sync transcription failed: %s", e, exc_info=True)
            raise RuntimeError(f"Transcription processing failed: {e}") from e

    def _load_audio(self, audio_path: Path) -> np.ndarray:
        """Load and normalize audio from WAV file.

        Handles sample rate conversion to 16 kHz and mono conversion as required by Whisper.

        Args:
            audio_path: Path to WAV file

        Returns:
            Audio data as float32 numpy array

        Raises:
            RuntimeError: If audio cannot be loaded or processed
        """
        try:
            import soundfile

            try:
                audio_data, sample_rate = soundfile.read(str(audio_path))
            except Exception:
                import wave

                with wave.open(str(audio_path), "rb") as wav_file:
                    sample_rate = wav_file.getframerate()
                    n_frames = wav_file.getnframes()
                    audio_data = np.frombuffer(
                        wav_file.readframes(n_frames), dtype=np.int16
                    ).astype(np.float32) / 32768.0

            logger.debug("Loaded audio: sample_rate=%d, shape=%s", sample_rate, audio_data.shape)

            if sample_rate != 16000:
                logger.debug("Resampling from %d Hz to 16 kHz", sample_rate)
                try:
                    from scipy.signal import resample_poly

                    ratio = 16000 / sample_rate
                    audio_data = resample_poly(
                        audio_data,
                        int(16000),
                        int(sample_rate),
                    )
                except ImportError:
                    logger.warning(
                        "scipy not available, using librosa for resampling"
                    )
                    import librosa

                    audio_data = librosa.resample(
                        audio_data,
                        orig_sr=sample_rate,
                        target_sr=16000,
                    )

            if audio_data.ndim > 1:
                logger.debug("Converting stereo to mono")
                audio_data = np.mean(audio_data, axis=1)

            audio_data = np.clip(audio_data, -1.0, 1.0).astype(np.float32)
            logger.debug("Audio normalized: shape=%s, dtype=%s", audio_data.shape, audio_data.dtype)
            return audio_data

        except Exception as e:
            logger.error("Failed to load audio from %s: %s", audio_path, e)
            raise RuntimeError(f"Failed to load audio from {audio_path}: {e}") from e

    def _normalize_text(self, text: str) -> str:
        """Post-process transcribed text.

        Applies cleaning: strip whitespace, remove duplicate spaces/newlines,
        and optional case normalization.

        Args:
            text: Raw transcribed text

        Returns:
            Normalized text
        """
        text = text.strip()
        text = re.sub(r"\s+", " ", text)
        text = re.sub(r"\n+", " ", text)
        text = re.sub(r"\.{2,}", ".", text)
        text = re.sub(r"\s+([.,!?;:])", r"\1", text)

        if len(text) > 0 and text[0].islower():
            text = text[0].upper() + text[1:]

        return text

    async def shutdown(self) -> None:
        """Clean up resources and shut down executor.

        Releases model reference and stops thread pool if owned by this instance.
        """
        logger.info("Transcriber shutting down")
        self._model = None
        if self._model_owned and self.executor:
            self.executor.shutdown(wait=True)
            logger.debug("Executor shut down")
