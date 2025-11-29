"""Configuration loader and validation."""

import logging
import sys
from collections.abc import Mapping
from dataclasses import dataclass, field
from pathlib import Path
from typing import Sequence

if sys.version_info >= (3, 11):
    import tomllib
else:
    import tomli as tomllib

logger = logging.getLogger(__name__)

__all__ = [
    "InputConfig",
    "AudioConfig",
    "ModelConfig",
    "DeepgramConfig",
    "InjectorConfig",
    "TranscriptionConfig",
    "OrchestratorConfig",
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

    device: str | Sequence[str]
    key_code: str | Sequence[str]
    devices: tuple[str, ...] = field(init=False)
    key_codes: tuple[str, ...] = field(init=False)

    def __post_init__(self) -> None:
        """Normalize device and key code configuration."""
        # Normalize devices
        if isinstance(self.device, str):
            devices = (self.device,)
        else:
            try:
                devices = tuple(self.device)
            except TypeError as exc:
                raise ConfigError(f"input.device must be a string or sequence of strings: {exc}") from exc

            if not devices:
                raise ConfigError("input.device must contain at least one device path")

        for dev in devices:
            if not isinstance(dev, str) or not dev:
                raise ConfigError("input.device entries must be non-empty strings")

        self.devices = devices
        self.device = devices[0]

        # Normalize key codes
        if isinstance(self.key_code, str):
            key_codes = (self.key_code,)
        else:
            try:
                key_codes = tuple(self.key_code)
            except TypeError as exc:
                raise ConfigError(f"input.key_code must be a string or sequence of strings: {exc}") from exc

            if not key_codes:
                raise ConfigError("input.key_code must contain at least one key symbol")

        for code in key_codes:
            if not isinstance(code, str) or not code:
                raise ConfigError("input.key_code entries must be non-empty strings")

        self.key_codes = key_codes
        self.key_code = key_codes[0]


@dataclass
class AudioConfig:
    """Audio capture configuration."""

    sample_rate: int = 16000
    channels: int = 1
    chunk_size: int = 4096
    device: int | str | None = None
    latency: float | str | None = None


@dataclass
class ModelConfig:
    """Whisper model configuration (for faster-whisper backend)."""

    backend: str = "faster_whisper"
    name: str = "base"
    device: str = "cpu"
    compute_type: str = "int8"
    model_directory: str | None = None
    beam_size: int = 5


@dataclass
class DeepgramConfig:
    """Deepgram API configuration (for deepgram backend)."""

    api_key: str | None = None
    model: str = "nova-3"
    smart_format: bool = True
    punctuate: bool = True
    utterances: bool = True
    timeout: float = 30.0


@dataclass
class InjectorConfig:
    """Text injection configuration."""

    backend: str = "wtype"
    clipboard_mode: bool = False
    typing_delay: int = 5
    use_newline: bool = False
    timeout: float = 10.0
    dry_run: bool = False


@dataclass
class TranscriptionConfig:
    """Transcription settings."""

    timeout: float = 30.0
    language: str = "en"
    noise_reduction: bool = False
    normalize_text: bool = True
    lowercase: bool = False
    capitalize_first: bool = True


@dataclass
class OrchestratorConfig:
    """Orchestrator state machine and coordination settings."""

    max_retries: int = 3
    error_recovery_delay: float = 1.0
    silence_recovery_timeout: float = 5.0
    recording_timeout: float = 300.0


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
    deepgram: DeepgramConfig
    injector: InjectorConfig
    transcription: TranscriptionConfig
    orchestrator: OrchestratorConfig = field(default_factory=OrchestratorConfig)
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
            coerced = _coerce_config_values(raw_data, env)
            return cls(
                input=InputConfig(**coerced.get("input", {})),
                audio=AudioConfig(**coerced.get("audio", {})),
                model=ModelConfig(**coerced.get("model", {})),
                deepgram=DeepgramConfig(**coerced.get("deepgram", {})),
                injector=InjectorConfig(**coerced.get("injector", {})),
                transcription=TranscriptionConfig(**coerced.get("transcription", {})),
                orchestrator=OrchestratorConfig(**coerced.get("orchestrator", {})),
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
            validate_input_device(self.input.devices)
            validate_audio_device(
                self.audio.sample_rate,
                self.audio.channels,
                self.audio.device,
            )
            validate_model_config(self.model)
            validate_deepgram_config(self.model, self.deepgram)
            validate_injector_config(self.injector)
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
        cli_path = Path(cli_path)
        if cli_path.exists():
            logger.info("Using config file: %s", cli_path.resolve())
            return cli_path.resolve()
        raise ConfigError(f"Config file not found: {cli_path}")

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


def _coerce_config_values(raw_data: dict, env: Mapping[str, str]) -> dict:
    """Normalize raw TOML data for dataclass instantiation.

    Handles type coercion and default fallback for optional sections.

    Args:
        raw_data: Raw parsed TOML dictionary
        env: Environment variables for overrides

    Returns:
        Coerced dictionary ready for dataclass instantiation
    """
    coerced = {}

    for section in ("input", "audio", "model", "deepgram", "injector", "transcription", "orchestrator", "general"):
        coerced[section] = raw_data.get(section, {})
        if not isinstance(coerced[section], dict):
            raise ConfigError(f"Section [{section}] must be a table")

    input_section = coerced["input"]

    if "input" in raw_data and not input_section.get("device"):
        raise ConfigError("input.device is required")

    if "key_codes" in input_section:
        key_codes_value = input_section["key_codes"]
        if not isinstance(key_codes_value, (list, tuple)):
            raise ConfigError("input.key_codes must be a list of key symbols")
        if not key_codes_value:
            raise ConfigError("input.key_codes cannot be empty")
        input_section["key_code"] = key_codes_value
        del input_section["key_codes"]

    if "input" in raw_data and not input_section.get("key_code"):
        raise ConfigError("input.key_code is required")

    # Handle Deepgram API key from environment variable if not in config
    deepgram_section = coerced["deepgram"]
    if not deepgram_section.get("api_key"):
        deepgram_section["api_key"] = env.get("DEEPGRAM_API_KEY")

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


def validate_input_device(device_path: str | Sequence[str]) -> None:
    """Validate that configured input device(s) exist and are accessible.

    Args:
        device_path: Path(s) to input device(s) (e.g., /dev/input/by-id/...)

    Raises:
        ConfigError: If device not found or not accessible
    """
    try:
        import evdev
    except ImportError:
        logger.warning("evdev not available, skipping input device validation")
        return

    # Normalize to list
    if isinstance(device_path, str):
        device_paths = [device_path]
    else:
        device_paths = list(device_path)

    # Validate each device
    for dev_path in device_paths:
        path = Path(dev_path)
        if not path.exists():
            available = discover_input_devices()
            device_list = (
                "\n  ".join(f"{d['path']} ({d['name']})" for d in available) or "none found"
            )
            raise ConfigError(
                f"Input device not found: {dev_path}\n"
                f"Available devices:\n  {device_list}"
            )

        try:
            evdev.InputDevice(dev_path)
        except (OSError, PermissionError) as e:
            raise ConfigError(
                f"Cannot access input device {dev_path}: {e}\n"
                f"Ensure you are in the 'input' group: groups | grep input"
            ) from e


def validate_audio_device(sample_rate: int, channels: int, device: int | str | None) -> None:
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
        if not _device_matches_selection(device, dev):
            continue
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


def _device_matches_selection(selection: int | str | None, device: dict) -> bool:
    """Check whether a device matches the selection criteria."""

    if selection is None:
        return True
    if isinstance(selection, int):
        return device["index"] == selection

    normalized = selection.strip().lower()
    candidate = device.get("name", "").strip().lower()

    if candidate == normalized:
        return True

    return normalized in candidate


def validate_model_config(model_cfg: ModelConfig) -> None:
    """Validate model configuration.

    Args:
        model_cfg: ModelConfig instance

    Raises:
        ConfigError: If model configuration is invalid
    """
    valid_backends = ("faster_whisper", "deepgram")
    if model_cfg.backend not in valid_backends:
        raise ConfigError(
            f"Invalid backend '{model_cfg.backend}'. "
            f"Must be one of: {', '.join(valid_backends)}"
        )

    valid_compute_types = ("int8", "float16", "float32", "default")
    if model_cfg.compute_type not in valid_compute_types:
        raise ConfigError(
            f"Invalid compute_type '{model_cfg.compute_type}'. "
            f"Must be one of: {', '.join(valid_compute_types)}"
        )

    valid_devices = ("cpu", "cuda", "auto")
    if model_cfg.device not in valid_devices:
        raise ConfigError(
            f"Invalid device '{model_cfg.device}'. "
            f"Must be one of: {', '.join(valid_devices)}"
        )

    if model_cfg.beam_size <= 0:
        raise ConfigError(f"beam_size must be positive, got {model_cfg.beam_size}")


def validate_deepgram_config(model_cfg: ModelConfig, deepgram_cfg: DeepgramConfig) -> None:
    """Validate Deepgram configuration when backend is deepgram.

    Args:
        model_cfg: ModelConfig instance (to check backend)
        deepgram_cfg: DeepgramConfig instance

    Raises:
        ConfigError: If Deepgram configuration is invalid
    """
    if model_cfg.backend != "deepgram":
        return

    if not deepgram_cfg.api_key:
        raise ConfigError(
            "Deepgram API key is required when backend is 'deepgram'. "
            "Set it in config file or via DEEPGRAM_API_KEY environment variable."
        )

    if deepgram_cfg.timeout <= 0:
        raise ConfigError(f"Deepgram timeout must be positive, got {deepgram_cfg.timeout}")


def validate_injector_config(injector_cfg: InjectorConfig) -> None:
    """Validate injector configuration.

    Args:
        injector_cfg: InjectorConfig instance

    Raises:
        ConfigError: If injector configuration is invalid
    """
    valid_backends = ("wtype", "ydotool", "xdotool")
    if injector_cfg.backend not in valid_backends:
        raise ConfigError(
            f"Invalid backend '{injector_cfg.backend}'. "
            f"Must be one of: {', '.join(valid_backends)}"
        )

    if injector_cfg.typing_delay < 0:
        raise ConfigError(
            f"typing_delay must be non-negative, got {injector_cfg.typing_delay}"
        )

    if injector_cfg.timeout <= 0:
        raise ConfigError(f"timeout must be positive, got {injector_cfg.timeout}")


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
