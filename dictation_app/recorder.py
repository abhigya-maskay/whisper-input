"""Audio capture and recording."""

import logging
import tempfile
import wave
from collections import deque
from enum import Enum
from pathlib import Path

import numpy as np
import sounddevice

logger = logging.getLogger(__name__)


class _RecorderState(Enum):
    """Internal recorder state machine."""

    IDLE = "idle"
    RECORDING = "recording"


class AudioRecorder:
    """Manages audio capture via sounddevice.

    Streams PCM audio into a buffer with optional silence trimming and format conversion.
    """

    def __init__(
        self,
        sample_rate: int = 16000,
        channels: int = 1,
        chunk_size: int = 4096,
        trim_silence: bool = False,
        silence_threshold: float = 0.02,
        silence_duration: float = 0.2,
        device: int | None = None,
    ):
        """Initialize audio recorder.

        Args:
            sample_rate: Sample rate in Hz
            channels: Number of channels
            chunk_size: Chunk size for streaming
            trim_silence: Whether to trim leading/trailing silence
            silence_threshold: RMS threshold for silence detection (0-1 range)
            silence_duration: Minimum silence duration in seconds to trim
        device: Audio device index or name (None for default)
        """
        if sample_rate <= 0:
            raise ValueError("sample_rate must be positive")
        if channels not in (1, 2):
            raise ValueError("channels must be 1 or 2")
        if chunk_size <= 0:
            raise ValueError("chunk_size must be positive")

        self.sample_rate = sample_rate
        self.channels = channels
        self.chunk_size = chunk_size
        self.trim_silence = trim_silence
        self.silence_threshold = silence_threshold
        self.silence_duration = silence_duration
        self.device = device

        self._state = _RecorderState.IDLE
        self._stream = None
        self._buffer = deque()
        self._temp_file = None

        logger.info(
            "AudioRecorder initialized: %d Hz, %d channels, device=%s",
            sample_rate,
            channels,
            device if device is not None else "default",
        )

    def __enter__(self):
        """Context manager entry."""
        return self

    def __exit__(self, exc_type, exc_val, exc_tb):
        """Context manager exit; ensure cleanup."""
        self.close()
        return False

    def start(self, token=None) -> None:
        """Start audio recording.

        Opens a sounddevice InputStream, registers callback, and begins buffering frames.

        Args:
            token: Optional RecordingCancellationToken to allow graceful abort

        Raises:
            RuntimeError: If already recording or stream cannot be opened
        """
        if self._state != _RecorderState.IDLE:
            raise RuntimeError(
                f"Cannot start recording: recorder in {self._state.value} state"
            )

        resolved_device = self._resolve_device_selection()

        try:
            self._buffer.clear()
            self._stream = sounddevice.InputStream(
                device=resolved_device,
                samplerate=self.sample_rate,
                channels=self.channels,
                blocksize=self.chunk_size,
                callback=self._callback,
                dtype="float32",
            )
            self._stream.start()
            self._state = _RecorderState.RECORDING
            logger.info(
                "Audio stream started (sample_rate=%d, channels=%d, device=%s)",
                self.sample_rate,
                self.channels,
                resolved_device if resolved_device is not None else "default",
            )
        except Exception as e:
            self._state = _RecorderState.IDLE
            logger.error("Failed to start audio stream: %s", e)
            raise RuntimeError(f"Failed to start audio stream: {e}") from e

    def stop(self) -> Path:
        """Stop recording and return path to saved audio file.

        Closes the stream, collects buffered frames, optionally trims silence,
        and writes WAV to a temporary file.

        Returns:
            Path to temporary WAV file

        Raises:
            RuntimeError: If not recording, no audio captured, or file write fails
        """
        if self._state != _RecorderState.RECORDING:
            raise RuntimeError(
                f"Cannot stop recording: recorder not recording (state={self._state.value})"
            )

        try:
            if self._stream:
                self._stream.stop()
                self._stream.close()
                self._stream = None

            if not self._buffer:
                raise RuntimeError("No audio frames captured")

            frames = np.concatenate(list(self._buffer))
            self._buffer.clear()

            if self.trim_silence:
                frames = self._trim_silence(frames)

            if len(frames) == 0:
                raise RuntimeError("Audio empty after silence trimming")

            self._temp_file = tempfile.NamedTemporaryFile(
                mode="w+b", suffix=".wav", delete=False
            )
            temp_path = Path(self._temp_file.name)
            self._temp_file.close()

            self._write_wav(temp_path, frames)
            self._state = _RecorderState.IDLE

            logger.info("Audio recording stopped and saved to %s", temp_path)
            return temp_path

        except RuntimeError:
            self._state = _RecorderState.IDLE
            raise
        except Exception as e:
            self._state = _RecorderState.IDLE
            logger.error("Failed to stop recording: %s", e)
            raise RuntimeError(f"Failed to stop recording: {e}") from e

    def close(self) -> None:
        """Explicitly close stream and cleanup resources."""
        if self._stream:
            try:
                self._stream.stop()
                self._stream.close()
            except Exception as e:
                logger.warning("Error closing stream: %s", e)
            finally:
                self._stream = None

        if self._temp_file and not self._temp_file.closed:
            try:
                self._temp_file.close()
            except Exception as e:
                logger.warning("Error closing temp file: %s", e)

        self._buffer.clear()
        self._state = _RecorderState.IDLE

    def _callback(self, indata, frames, time_info, status):
        """Stream callback invoked on audio data arrival.

        Args:
            indata: numpy array of audio data
            frames: number of frames
            time_info: timing information
            status: stream status flags
        """
        if status:
            logger.warning("Audio stream status: %s", status)

        self._buffer.append(indata.copy())

    def _trim_silence(self, frames: np.ndarray) -> np.ndarray:
        """Trim leading and trailing silence from audio frames.

        Uses RMS energy threshold to detect silence. Only trims if result
        would be non-empty.

        Args:
            frames: Audio frames as numpy array (float32, normalized [-1, 1])

        Returns:
            Trimmed audio frames
        """
        min_silence_samples = int(self.silence_duration * self.sample_rate)

        if len(frames) < min_silence_samples * 2:
            return frames

        rms_values = self._compute_rms(frames)

        # Find non-silent regions
        loud_mask = rms_values > self.silence_threshold
        loud_indices = np.where(loud_mask)[0]

        if len(loud_indices) == 0:
            logger.debug("All audio is below silence threshold")
            return frames

        # Get first and last loud chunks
        first_loud_chunk = loud_indices[0]
        last_loud_chunk = loud_indices[-1]

        # Convert chunk indices to sample indices
        chunk_size = self.sample_rate // 10
        if chunk_size == 0:
            chunk_size = 1

        start_idx = max(0, first_loud_chunk * chunk_size - min_silence_samples)
        end_idx = min(
            len(frames), (last_loud_chunk + 1) * chunk_size + min_silence_samples
        )

        trimmed = frames[start_idx:end_idx]
        logger.debug(
            "Trimmed silence: %d → %d samples",
            len(frames),
            len(trimmed),
        )
        return trimmed

    def _compute_rms(self, frames: np.ndarray) -> np.ndarray:
        """Compute RMS energy per chunk.

        Args:
            frames: Audio frames

        Returns:
            Array of RMS values per chunk
        """
        chunk_samples = self.sample_rate // 10
        if chunk_samples == 0:
            chunk_samples = 1

        rms_values = []
        for i in range(0, len(frames), chunk_samples):
            chunk = frames[i : i + chunk_samples]
            rms = np.sqrt(np.mean(chunk**2))
            rms_values.append(rms)

        return np.array(rms_values)

    def _write_wav(self, path: Path, frames: np.ndarray) -> None:
        """Write audio frames to WAV file.

        Args:
            path: Output file path
            frames: Audio frames (float32, normalized [-1, 1])

        Raises:
            RuntimeError: If file write fails
        """
        try:
            with wave.open(str(path), "wb") as wav_file:
                wav_file.setnchannels(self.channels)
                wav_file.setsampwidth(2)
                wav_file.setframerate(self.sample_rate)

                audio_int16 = np.clip(frames * 32767, -32768, 32767).astype(np.int16)
                wav_file.writeframes(audio_int16.tobytes())

        except Exception as e:
            logger.error("Failed to write WAV file: %s", e)
            raise RuntimeError(f"Failed to write WAV file: {e}") from e

    @staticmethod
    def list_devices() -> dict[int, str]:
        """List available audio capture devices.

        Queries sounddevice and filters for devices with input capability.

        Returns:
            Dict mapping device index to name (e.g., {0: "Default", 2: "USB Audio"})
            Empty dict if no devices found or error occurs
        """
        try:
            devices = sounddevice.query_devices()

            if isinstance(devices, dict):
                devices = [devices]

            result = {}
            for idx, dev_info in enumerate(devices):
                if dev_info.get("max_input_channels", 0) > 0:
                    result[idx] = dev_info.get("name", f"Device {idx}")

            logger.debug("Found %d audio input devices", len(result))
            return result

        except sounddevice.PortAudioError as e:
            logger.warning("PortAudio error querying devices: %s", e)
            return {}
        except Exception as e:
            logger.warning("Error querying audio devices: %s", e)
            return {}

    def _resolve_device_selection(self) -> int | None:
        """Resolve configured device selection to a sounddevice index."""

        if self.device is None or isinstance(self.device, int):
            return self.device

        try:
            device_list = sounddevice.query_devices()
            if isinstance(device_list, dict):
                device_list = [device_list]
        except Exception as e:
            logger.warning(
                "Unable to enumerate audio devices for '%s': %s; using default",
                self.device,
                e,
            )
            return None

        target = self.device.strip().lower()
        partial_matches: list[tuple[int, str]] = []
        available: list[str] = []

        for idx, dev_info in enumerate(device_list):
            if dev_info.get("max_input_channels", 0) <= 0:
                continue

            name = dev_info.get("name", f"Device {idx}")
            normalized = name.strip().lower()
            available.append(f"[{idx}] {name}")

            if normalized == target:
                logger.debug(
                    "Resolved audio device '%s' to index %d (exact match)",
                    self.device,
                    idx,
                )
                return idx

            if target in normalized:
                partial_matches.append((idx, name))

        if partial_matches:
            idx, name = partial_matches[0]
            logger.debug(
                "Resolved audio device '%s' to index %d via partial match (%s)",
                self.device,
                idx,
                name,
            )
            return idx

        logger.warning(
            "Audio device '%s' not found. Using default input. Available devices: %s",
            self.device,
            "; ".join(available) if available else "none",
        )
        return None
