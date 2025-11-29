"""Audio transcription via Deepgram API."""

import asyncio
import logging
import re
import time
from concurrent.futures import ThreadPoolExecutor
from pathlib import Path

from dictation_app._types import TranscriptionResult, TranscriptionSegment

logger = logging.getLogger(__name__)


class DeepgramTranscriber:
    """Encapsulates Deepgram API client and transcription logic.

    Runs transcription inside a thread pool executor to avoid blocking the event loop.
    Lazy-initializes client on first transcription.
    """

    def __init__(
        self,
        api_key: str,
        model: str = "nova-3",
        smart_format: bool = True,
        punctuate: bool = True,
        utterances: bool = True,
        timeout: float = 30.0,
        executor: ThreadPoolExecutor | None = None,
        lowercase: bool = False,
        capitalize_first: bool = True,
    ):
        """Initialize Deepgram transcriber.

        Args:
            api_key: Deepgram API key
            model: Deepgram model (nova-3, nova-2, whisper-large, etc.)
            smart_format: Enable smart formatting (currency, dates, etc.)
            punctuate: Auto-add punctuation
            utterances: Return utterance-level timestamps
            timeout: API request timeout in seconds
            executor: Optional ThreadPoolExecutor for transcription tasks
            lowercase: Convert output to lowercase
            capitalize_first: Capitalize first letter of output
        """
        self.api_key = api_key
        self.model = model
        self.smart_format = smart_format
        self.punctuate = punctuate
        self.utterances = utterances
        self.timeout = timeout
        self.executor = executor or ThreadPoolExecutor(max_workers=1)
        self._executor_owned = executor is None
        self._client = None
        self._client_lock = asyncio.Lock()
        self.lowercase = lowercase
        self.capitalize_first = capitalize_first
        logger.info(
            "DeepgramTranscriber initialized: model=%s, smart_format=%s, punctuate=%s, "
            "lowercase=%s, capitalize_first=%s",
            model,
            smart_format,
            punctuate,
            lowercase,
            capitalize_first,
        )

    async def _ensure_client_initialized(self) -> None:
        """Lazy-initialize Deepgram client on first use.

        Uses asyncio.Lock to prevent concurrent initialization attempts.

        Raises:
            RuntimeError: If client initialization fails
        """
        async with self._client_lock:
            if self._client is not None:
                return

            logger.info("Initializing Deepgram client with model: %s", self.model)

            try:
                from deepgram import DeepgramClient

                start_time = time.perf_counter()
                self._client = DeepgramClient(api_key=self.api_key)
                duration = time.perf_counter() - start_time
                logger.info("Deepgram client initialized in %.3f seconds", duration)
            except Exception as e:
                logger.error("Failed to initialize Deepgram client: %s", e)
                raise RuntimeError(f"Failed to initialize Deepgram client: {e}") from e

    async def transcribe(
        self,
        audio_path: Path,
        language: str = "en",
        timeout: float = 30.0,
    ) -> TranscriptionResult:
        """Transcribe audio file asynchronously using Deepgram API.

        Initializes client lazily on first call, runs transcription in thread pool
        to avoid blocking the event loop, and applies text post-processing.

        Args:
            audio_path: Path to audio file (WAV, MP3, FLAC, etc.)
            language: Language code (e.g., "en", "fr") or "auto" for detection
            timeout: Maximum time in seconds

        Returns:
            TranscriptionResult with text, language, and segments

        Raises:
            RuntimeError: If client initialization or transcription fails
            asyncio.TimeoutError: If transcription exceeds timeout
        """
        audio_path = Path(audio_path)
        if not audio_path.exists():
            raise RuntimeError(f"Audio file not found: {audio_path}")

        await self._ensure_client_initialized()

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
        """Synchronous transcription using Deepgram API (runs in thread pool).

        Args:
            audio_path: Path to audio file
            language: Language code

        Returns:
            TranscriptionResult with text and segments

        Raises:
            RuntimeError: If transcription fails
        """
        try:
            if self._client is None:
                raise RuntimeError("Deepgram client not initialized")

            # Read audio file as bytes
            with open(audio_path, "rb") as audio_file:
                audio_bytes = audio_file.read()

            # Configure transcription options
            options = {
                "model": self.model,
                "smart_format": self.smart_format,
                "punctuate": self.punctuate,
                "utterances": self.utterances,
            }

            # Add language if not auto-detect
            if language != "auto":
                options["language"] = language

            logger.debug("Deepgram options: %s", options)

            # Make API call
            from deepgram.core.api_error import ApiError

            try:
                response = self._client.listen.v1.media.transcribe_file(
                    request=audio_bytes,
                    **options
                )
            except ApiError as e:
                if e.status_code == 401:
                    raise RuntimeError("Invalid Deepgram API key") from e
                elif e.status_code == 429:
                    raise RuntimeError("Deepgram API rate limit exceeded") from e
                elif e.status_code >= 500:
                    raise RuntimeError(f"Deepgram server error: {e.status_code}") from e
                else:
                    raise RuntimeError(f"Deepgram API error ({e.status_code}): {e.body}") from e

            # Extract transcription from response
            channel = response.results.channels[0]
            alternative = channel.alternatives[0]
            text = alternative.transcript

            # Get detected language
            detected_language = language
            if language == "auto" and hasattr(channel, "detected_language"):
                detected_language = channel.detected_language or "en"

            # Extract segments from utterances if available
            segments = []
            if self.utterances and hasattr(response.results, "utterances"):
                for utt in response.results.utterances:
                    segments.append(
                        TranscriptionSegment(
                            text=utt.transcript,
                            start=utt.start,
                            end=utt.end,
                            confidence=utt.confidence if hasattr(utt, "confidence") else 0.0,
                        )
                    )
            elif hasattr(alternative, "words") and alternative.words:
                # Fallback: create segments from words (group by ~5 second chunks)
                current_segment_words = []
                current_start = None
                current_end = None

                for word in alternative.words:
                    if current_start is None:
                        current_start = word.start

                    current_segment_words.append(word.word)
                    current_end = word.end

                    # Create segment every ~5 seconds or at end
                    if (current_end - current_start >= 5.0) or (word == alternative.words[-1]):
                        segments.append(
                            TranscriptionSegment(
                                text=" ".join(current_segment_words),
                                start=current_start,
                                end=current_end,
                                confidence=word.confidence if hasattr(word, "confidence") else 0.0,
                            )
                        )
                        current_segment_words = []
                        current_start = None
                        current_end = None

            # Get overall confidence if available
            confidence = alternative.confidence if hasattr(alternative, "confidence") else 0.0

            # Normalize text
            text = self._normalize_text(text)

            return TranscriptionResult(
                text=text,
                language=detected_language,
                confidence=confidence,
                segments=segments,
            )

        except Exception as e:
            logger.error("Sync transcription failed: %s", e, exc_info=True)
            raise RuntimeError(f"Transcription processing failed: {e}") from e

    def _normalize_text(self, text: str) -> str:
        """Post-process transcribed text.

        Applies cleaning: strip whitespace, remove duplicate spaces/newlines,
        and optional case normalization based on config settings.

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

        # Apply case transformations
        if self.lowercase:
            text = text.lower()
        elif self.capitalize_first and len(text) > 0 and text[0].islower():
            text = text[0].upper() + text[1:]

        return text

    async def shutdown(self) -> None:
        """Clean up resources and shut down executor.

        Releases client reference and stops thread pool if owned by this instance.
        """
        logger.info("DeepgramTranscriber shutting down")
        self._client = None
        if self._executor_owned and self.executor:
            self.executor.shutdown(wait=True)
            logger.debug("Executor shut down")
