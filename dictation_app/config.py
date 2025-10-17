"""Configuration loader and validation."""

from dataclasses import dataclass
from pathlib import Path


@dataclass
class InputConfig:
    """Input device configuration."""

    device: str
    key_code: str

    # TODO: Add validation and parsing logic


@dataclass
class AudioConfig:
    """Audio capture configuration."""

    sample_rate: int
    channels: int
    chunk_size: int

    # TODO: Add validation and default values


@dataclass
class ModelConfig:
    """Whisper model configuration."""

    name: str
    device: str
    compute_type: str

    # TODO: Add validation and model loading options


@dataclass
class InjectorConfig:
    """Text injection configuration."""

    backend: str
    clipboard_mode: bool = False

    # TODO: Add validation and backend selection


@dataclass
class TranscriptionConfig:
    """Transcription settings."""

    timeout: float
    language: str
    noise_reduction: bool = False

    # TODO: Add post-processing options


@dataclass
class Config:
    """Main configuration container."""

    input: InputConfig
    audio: AudioConfig
    model: ModelConfig
    injector: InjectorConfig
    transcription: TranscriptionConfig

    # TODO: Implement from_toml() classmethod to load from dictation.toml
    # TODO: Implement validation and error handling
