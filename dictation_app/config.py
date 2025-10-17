"""Configuration loader and validation."""

import logging
import sys
from collections.abc import Mapping
from dataclasses import dataclass, field
from pathlib import Path

if sys.version_info >= (3, 11):
    import tomllib
else:
    import tomli as tomllib

logger = logging.getLogger(__name__)

__all__ = [
    "InputConfig",
    "AudioConfig",
    "ModelConfig",
    "InjectorConfig",
    "TranscriptionConfig",
    "GeneralConfig",
    "Config",
    "ConfigError",
    "load_config",
    "discover_input_devices",
    "discover_audio_devices",
]


class ConfigError(Exception):
    """Configuration loading or validation error."""

    pass


@dataclass
class InputConfig:
    """Input device configuration."""

    device: str
    key_code: str


@dataclass
class AudioConfig:
    """Audio capture configuration."""

    sample_rate: int = 16000
    channels: int = 1
    chunk_size: int = 4096


@dataclass
class ModelConfig:
    """Whisper model configuration."""

    name: str = "base"
    device: str = "cpu"
    compute_type: str = "default"


@dataclass
class InjectorConfig:
    """Text injection configuration."""

    backend: str = "wtype"
    clipboard_mode: bool = False


@dataclass
class TranscriptionConfig:
    """Transcription settings."""

    timeout: float = 30.0
    language: str = "en"
    noise_reduction: bool = False


@dataclass
class GeneralConfig:
    """General application settings."""

    verbose: bool = False
    debug: bool = False


@dataclass
class Config:
    """Main configuration container."""

    input: InputConfig
    audio: AudioConfig
    model: ModelConfig
    injector: InjectorConfig
    transcription: TranscriptionConfig
    general: GeneralConfig = field(default_factory=GeneralConfig)

    @classmethod
    def from_toml(
        cls,
        path: Path | None = None,
        *,
        env: Mapping[str, str] | None = None,
    ) -> "Config":
        """Load configuration from TOML file with environment overrides.

        Args:
            path: Explicit config file path. If None, searches in order:
                  1. DICTATION_CONFIG env var
                  2. ./dictation.toml
                  3. ~/.config/dictation.toml
            env: Environment variables for overrides (defaults to os.environ)

        Returns:
            Loaded and validated Config instance

        Raises:
            ConfigError: If config file not found or validation fails
        """
        if env is None:
            import os

            env = os.environ

        resolved_path = _resolve_config_path(path, env)
        raw_data = _load_toml_file(resolved_path)

        try:
            coerced = _coerce_config_values(raw_data)
            return cls(
                input=InputConfig(**coerced.get("input", {})),
                audio=AudioConfig(**coerced.get("audio", {})),
                model=ModelConfig(**coerced.get("model", {})),
                injector=InjectorConfig(**coerced.get("injector", {})),
                transcription=TranscriptionConfig(**coerced.get("transcription", {})),
                general=GeneralConfig(**coerced.get("general", {})),
            )
        except TypeError as e:
            raise ConfigError(f"Invalid configuration values: {e}") from e

    def validate(self) -> None:
        """Validate configuration against available hardware.

        Raises:
            ConfigError: If configured devices unavailable or invalid
        """
        try:
            validate_input_device(self.input.device)
            validate_audio_device(
                self.audio.sample_rate,
                self.audio.channels,
            )
        except ConfigError:
            raise
        except Exception as e:
            raise ConfigError(f"Validation failed: {e}") from e


def _resolve_config_path(
    cli_path: Path | None,
    env: Mapping[str, str],
) -> Path:
    """Resolve configuration file path following search order.

    Search order:
    1. CLI-provided path
    2. DICTATION_CONFIG environment variable
    3. ./dictation.toml (current directory)
    4. ~/.config/dictation.toml (user config directory)

    Raises:
        ConfigError: If no config file found in any location
    """
    candidates = []

    if cli_path:
        candidates.append(cli_path)

    if env_path := env.get("DICTATION_CONFIG"):
        candidates.append(Path(env_path))

    candidates.append(Path("dictation.toml"))
    candidates.append(Path.home() / ".config" / "dictation.toml")

    for candidate in candidates:
        if candidate.exists():
            logger.info("Using config file: %s", candidate.resolve())
            return candidate.resolve()

    raise ConfigError(
        f"Config file not found. Searched: {', '.join(str(c) for c in candidates)}"
    )


def _load_toml_file(path: Path) -> dict:
    """Load and parse TOML configuration file.

    Args:
        path: Path to TOML file

    Returns:
        Parsed TOML data as dictionary

    Raises:
        ConfigError: If file cannot be read or parsed
    """
    try:
        with open(path, "rb") as f:
            return tomllib.load(f)
    except FileNotFoundError as e:
        raise ConfigError(f"Config file not found: {path}") from e
    except Exception as e:
        raise ConfigError(f"Failed to parse config file {path}: {e}") from e


def _coerce_config_values(raw_data: dict) -> dict:
    """Normalize raw TOML data for dataclass instantiation.

    Handles type coercion and default fallback for optional sections.

    Args:
        raw_data: Raw parsed TOML dictionary

    Returns:
        Coerced dictionary ready for dataclass instantiation
    """
    coerced = {}

    for section in ("input", "audio", "model", "injector", "transcription", "general"):
        coerced[section] = raw_data.get(section, {})
        if not isinstance(coerced[section], dict):
            raise ConfigError(f"Section [{section}] must be a table")

    if "input" in raw_data and not raw_data["input"].get("device"):
        raise ConfigError("input.device is required")
    if "input" in raw_data and not raw_data["input"].get("key_code"):
        raise ConfigError("input.key_code is required")

    return coerced


def discover_input_devices() -> list[dict]:
    """Enumerate available input devices via evdev.

    Returns:
        List of device dicts with keys: path, name, capabilities
        Returns empty list if evdev unavailable or no devices found
    """
    try:
        import evdev
    except ImportError:
        logger.warning("evdev not available, cannot enumerate input devices")
        return []

    devices = []
    try:
        input_dir = Path("/dev/input/by-id")
        if not input_dir.exists():
            logger.warning("/dev/input/by-id not found")
            return devices

        for device_path in sorted(input_dir.iterdir()):
            try:
                dev = evdev.InputDevice(str(device_path))
                devices.append(
                    {
                        "path": str(device_path.resolve()),
                        "name": dev.name,
                        "capabilities": list(dev.capabilities().keys()),
                    }
                )
            except (OSError, PermissionError) as e:
                logger.debug("Cannot access device %s: %s", device_path, e)
    except Exception as e:
        logger.warning("Error discovering input devices: %s", e)

    return devices


def discover_audio_devices() -> list[dict]:
    """Enumerate available audio capture devices.

    Returns:
        List of device dicts with keys: index, name, channels, sample_rate
        Returns empty list if sounddevice unavailable or no devices found
    """
    try:
        import sounddevice
    except ImportError:
        logger.warning("sounddevice not available, cannot enumerate audio devices")
        return []

    devices = []
    try:
        device_list = sounddevice.query_devices()
        if isinstance(device_list, dict):
            device_list = [device_list]

        for idx, dev_info in enumerate(device_list):
            if dev_info.get("max_input_channels", 0) > 0:
                devices.append(
                    {
                        "index": idx,
                        "name": dev_info.get("name", f"Device {idx}"),
                        "channels": dev_info.get("max_input_channels", 0),
                        "sample_rate": dev_info.get("default_samplerate", 0),
                    }
                )
    except Exception as e:
        logger.warning("Error discovering audio devices: %s", e)

    return devices


def validate_input_device(device_path: str) -> None:
    """Validate that configured input device exists and is accessible.

    Args:
        device_path: Path to input device (e.g., /dev/input/by-id/...)

    Raises:
        ConfigError: If device not found or not accessible
    """
    try:
        import evdev
    except ImportError:
        logger.warning("evdev not available, skipping input device validation")
        return

    path = Path(device_path)
    if not path.exists():
        available = discover_input_devices()
        device_list = (
            "\n  ".join(f"{d['path']} ({d['name']})" for d in available) or "none found"
        )
        raise ConfigError(
            f"Input device not found: {device_path}\n"
            f"Available devices:\n  {device_list}"
        )

    try:
        evdev.InputDevice(device_path)
    except (OSError, PermissionError) as e:
        raise ConfigError(
            f"Cannot access input device {device_path}: {e}\n"
            f"Ensure you are in the 'input' group: groups | grep input"
        ) from e


def validate_audio_device(sample_rate: int, channels: int) -> None:
    """Validate that audio configuration is supported.

    Args:
        sample_rate: Requested sample rate in Hz
        channels: Requested number of channels

    Raises:
        ConfigError: If no suitable audio device found
    """
    try:
        import sounddevice
    except ImportError:
        logger.warning("sounddevice not available, skipping audio device validation")
        return

    devices = discover_audio_devices()
    if not devices:
        logger.warning("No audio capture devices found")
        return

    for dev in devices:
        if dev["channels"] >= channels and dev["sample_rate"] > 0:
            logger.debug(
                "Audio device validated: %s (%dHz, %d channels)",
                dev["name"],
                dev["sample_rate"],
                dev["channels"],
            )
            return

    device_list = "\n  ".join(
        f"{d['name']} ({d['channels']} ch, {d['sample_rate']}Hz)" for d in devices
    )
    raise ConfigError(
        f"No audio device supports {channels} channels at {sample_rate}Hz\n"
        f"Available devices:\n  {device_list}"
    )


def load_config(
    path: Path | None = None,
    *,
    env: Mapping[str, str] | None = None,
) -> Config:
    """Load configuration from TOML file.

    Convenience wrapper around Config.from_toml().

    Args:
        path: Explicit config file path (optional)
        env: Environment variables (defaults to os.environ)

    Returns:
        Loaded Config instance

    Raises:
        ConfigError: If config cannot be loaded or validated
    """
    return Config.from_toml(path, env=env)
