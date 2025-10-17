"""Shared types and dataclasses for cross-module use."""

from dataclasses import dataclass, field


@dataclass
class AudioChunk:
    """Container for audio data chunk."""

    data: bytes
    sample_rate: int
    channels: int


@dataclass
class TranscriptionSegment:
    """A segment of transcribed text with timing information."""

    text: str
    start: float
    end: float
    confidence: float = 0.0


@dataclass
class TranscriptionResult:
    """Result from transcription."""

    text: str
    language: str
    confidence: float = 0.0
    segments: list[TranscriptionSegment] = field(default_factory=list)
