"""Shared types and dataclasses for cross-module use."""

from dataclasses import dataclass


@dataclass
class AudioChunk:
    """Container for audio data chunk."""

    data: bytes
    sample_rate: int
    channels: int

    # TODO: Add format and other metadata as needed


@dataclass
class TranscriptionResult:
    """Result from transcription."""

    text: str
    language: str
    confidence: float = 0.0

    # TODO: Add segments, timing info as needed
