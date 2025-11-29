"""Tests for config module."""

import tempfile
from pathlib import Path
from unittest.mock import MagicMock, patch

import pytest

from dictation_app.config import (
    AudioConfig,
    Config,
    ConfigError,
    GeneralConfig,
    InjectorConfig,
    InputConfig,
    ModelConfig,
    TranscriptionConfig,
    discover_audio_devices,
    discover_input_devices,
    load_config,
    validate_audio_device,
    validate_input_device,
)


@pytest.fixture
def tmp_config_file():
    """Create a temporary TOML config file for testing."""

    def _create(content: str) -> Path:
        with tempfile.NamedTemporaryFile(mode="w", suffix=".toml", delete=False) as f:
            f.write(content)
            return Path(f.name)

    return _create


@pytest.fixture
def minimal_config_content():
    """Minimal valid configuration content."""
    return """
[input]
device = "/dev/input/test"
key_code = "BTN_EXTRA"
"""


@pytest.fixture
def full_config_content():
    """Full configuration with all sections."""
    return """
[input]
device = "/dev/input/test"
key_code = "BTN_EXTRA"

[audio]
sample_rate = 16000
channels = 1
chunk_size = 4096

[model]
name = "base"
device = "cpu"
compute_type = "default"

[injector]
backend = "wtype"
clipboard_mode = false

[transcription]
timeout = 30.0
language = "en"
noise_reduction = false

[general]
verbose = false
debug = false
"""


class TestConfigDataclasses:
    """Test configuration dataclasses."""

    def test_input_config_creation(self):
        """Test InputConfig instantiation."""
        cfg = InputConfig(device="/dev/input/test", key_code="BTN_EXTRA")
        assert cfg.device == "/dev/input/test"
        assert cfg.key_code == "BTN_EXTRA"
        assert cfg.key_codes == ("BTN_EXTRA",)

    def test_input_config_multiple_codes(self):
        """Test InputConfig with multiple key codes."""
        cfg = InputConfig(
            device="/dev/input/test",
            key_code=["BTN_EXTRA", "BTN_THUMB2"],
        )
        assert cfg.key_codes == ("BTN_EXTRA", "BTN_THUMB2")
        assert cfg.key_code == "BTN_EXTRA"

    def test_audio_config_defaults(self):
        """Test AudioConfig defaults."""
        cfg = AudioConfig()
        assert cfg.sample_rate == 16000
        assert cfg.channels == 1
        assert cfg.chunk_size == 4096

    def test_model_config_defaults(self):
        """Test ModelConfig defaults."""
        cfg = ModelConfig()
        assert cfg.name == "base"
        assert cfg.device == "cpu"
        assert cfg.compute_type == "int8"

    def test_injector_config_defaults(self):
        """Test InjectorConfig defaults."""
        cfg = InjectorConfig()
        assert cfg.backend == "wtype"
        assert cfg.clipboard_mode is False

    def test_transcription_config_defaults(self):
        """Test TranscriptionConfig defaults."""
        cfg = TranscriptionConfig()
        assert cfg.timeout == 30.0
        assert cfg.language == "en"
        assert cfg.noise_reduction is False

    def test_general_config_defaults(self):
        """Test GeneralConfig defaults."""
        cfg = GeneralConfig()
        assert cfg.verbose is False
        assert cfg.debug is False


class TestConfigLoading:
    """Test config file loading."""

    def test_load_config_from_explicit_path(self, tmp_config_file, full_config_content):
        """Test loading config from explicit path."""
        cfg_path = tmp_config_file(full_config_content)
        try:
            cfg = load_config(cfg_path)
            assert cfg.input.device == "/dev/input/test"
            assert cfg.input.key_code == "BTN_EXTRA"
            assert cfg.audio.sample_rate == 16000
        finally:
            cfg_path.unlink()

    def test_load_config_minimal(self, tmp_config_file, minimal_config_content):
        """Test loading minimal config with defaults."""
        cfg_path = tmp_config_file(minimal_config_content)
        try:
            cfg = load_config(cfg_path)
            assert cfg.input.device == "/dev/input/test"
            assert cfg.audio.sample_rate == 16000
            assert cfg.model.name == "base"
        finally:
            cfg_path.unlink()

    def test_load_config_from_toml_classmethod(
        self, tmp_config_file, full_config_content
    ):
        """Test Config.from_toml() classmethod."""
        cfg_path = tmp_config_file(full_config_content)
        try:
            cfg = Config.from_toml(cfg_path)
            assert isinstance(cfg, Config)
            assert cfg.input.device == "/dev/input/test"
        finally:
            cfg_path.unlink()

    def test_load_config_file_not_found(self):
        """Test error when config file not found."""
        with pytest.raises(ConfigError, match="Config file not found"):
            load_config(Path("/nonexistent/config.toml"))

    def test_load_config_invalid_toml(self, tmp_config_file):
        """Test error on invalid TOML syntax."""
        invalid_content = "[input\ndevice = missing quote"
        cfg_path = tmp_config_file(invalid_content)
        try:
            with pytest.raises(ConfigError, match="Failed to parse"):
                load_config(cfg_path)
        finally:
            cfg_path.unlink()

    def test_load_config_missing_required_input_device(self, tmp_config_file):
        """Test error when required input.device is missing."""
        content = """
[input]
key_code = "BTN_EXTRA"
"""
        cfg_path = tmp_config_file(content)
        try:
            with pytest.raises(ConfigError, match="input.device is required"):
                load_config(cfg_path)
        finally:
            cfg_path.unlink()

    def test_load_config_env_var_override(self, tmp_config_file, full_config_content):
        """Test environment variable config path override."""
        cfg_path = tmp_config_file(full_config_content)
        try:
            env = {"DICTATION_CONFIG": str(cfg_path)}
            cfg = load_config(None, env=env)
            assert cfg.input.device == "/dev/input/test"
        finally:
            cfg_path.unlink()

    def test_load_config_cli_path_takes_precedence(
        self, tmp_config_file, full_config_content
    ):
        """Test that CLI path takes precedence over env var."""
        cfg_path1 = tmp_config_file(full_config_content)
        cfg_path2 = tmp_config_file(full_config_content)
        try:
            env = {"DICTATION_CONFIG": str(cfg_path1)}
            cfg = load_config(cfg_path2, env=env)
            assert cfg.input.device == "/dev/input/test"
        finally:
            cfg_path1.unlink()
            cfg_path2.unlink()


class TestConfigValidation:
    """Test configuration validation."""

    @patch("dictation_app.config.Path.exists")
    def test_validate_input_device_not_found(self, mock_exists):
        """Test validation when input device doesn't exist."""
        mock_exists.return_value = False

        with patch("dictation_app.config.discover_input_devices", return_value=[]):
            with pytest.raises(ConfigError, match="Input device not found"):
                validate_input_device("/dev/input/nonexistent")

    def test_validate_input_device_permission_denied(self):
        """Test validation when device exists but not accessible."""
        from pathlib import Path as RealPath

        with patch("dictation_app.config.Path.exists") as mock_exists:
            mock_exists.return_value = True

            with patch("evdev.InputDevice") as mock_input_device:
                mock_input_device.side_effect = PermissionError("Permission denied")

                with pytest.raises(ConfigError, match="Cannot access input device"):
                    validate_input_device("/dev/input/test")

    @patch("dictation_app.config.discover_audio_devices")
    def test_validate_audio_device_no_devices(self, mock_discover):
        """Test validation when no audio devices found."""
        mock_discover.return_value = []
        validate_audio_device(16000, 1, None)  # Should not raise

    @patch("dictation_app.config.discover_audio_devices")
    def test_validate_audio_device_unsupported_config(self, mock_discover):
        """Test validation when audio config unsupported."""
        mock_discover.return_value = [
            {"index": 0, "name": "Device", "channels": 1, "sample_rate": 48000}
        ]

        with pytest.raises(ConfigError, match="No audio device supports"):
            validate_audio_device(16000, 2, None)

    @patch("dictation_app.config.discover_audio_devices")
    def test_validate_audio_device_success(self, mock_discover):
        """Test successful audio device validation."""
        mock_discover.return_value = [
            {"index": 0, "name": "Device", "channels": 2, "sample_rate": 48000}
        ]

        validate_audio_device(16000, 1, None)  # Should not raise


class TestDiscoveryFunctions:
    """Test device discovery functions."""

    def test_discover_input_devices(self):
        """Test input device discovery."""
        with patch("evdev.InputDevice") as mock_input_device:
            mock_dev = MagicMock()
            mock_dev.name = "Test Keyboard"
            mock_dev.capabilities.return_value = {1: "KEY_CODES"}
            mock_input_device.return_value = mock_dev

            with patch.object(Path, "exists", return_value=True):
                with patch.object(
                    Path, "iterdir", return_value=[Path("/dev/input/by-id/test")]
                ):
                    devices = discover_input_devices()
                    assert len(devices) > 0
                    assert devices[0]["name"] == "Test Keyboard"

    def test_discover_input_devices_evdev_unavailable(self):
        """Test graceful fallback when evdev unavailable."""
        with patch.dict("sys.modules", {"evdev": None}):
            with patch("dictation_app.config.logger.warning") as mock_warning:
                devices = discover_input_devices()
                assert devices == []

    def test_discover_audio_devices(self):
        """Test audio device discovery."""
        with patch("sounddevice.query_devices") as mock_query:
            mock_query.return_value = [
                {
                    "name": "Microphone",
                    "max_input_channels": 2,
                    "default_samplerate": 48000,
                }
            ]

            devices = discover_audio_devices()
            assert len(devices) == 1
            assert devices[0]["name"] == "Microphone"
            assert devices[0]["channels"] == 2

    def test_discover_audio_devices_unavailable(self):
        """Test graceful fallback when sounddevice unavailable."""
        with patch.dict("sys.modules", {"sounddevice": None}):
            devices = discover_audio_devices()
            assert devices == []


class TestConfigIntegration:
    """Integration tests for full config workflow."""

    def test_config_load_and_validate_workflow(
        self, tmp_config_file, full_config_content
    ):
        """Test complete load and validate workflow."""
        cfg_path = tmp_config_file(full_config_content)
        try:
            with patch("dictation_app.config.validate_input_device"):
                with patch("dictation_app.config.validate_audio_device"):
                    cfg = load_config(cfg_path)
                    cfg.validate()
        finally:
            cfg_path.unlink()

    def test_config_custom_values(self, tmp_config_file):
        """Test loading custom configuration values."""
        content = """
[input]
device = "/dev/input/custom"
key_code = "BTN_SIDE"

[audio]
sample_rate = 48000
channels = 2
chunk_size = 8192

[model]
name = "small"
device = "cuda"
compute_type = "float16"

[transcription]
timeout = 60.0
language = "fr"
noise_reduction = true
"""
        cfg_path = tmp_config_file(content)
        try:
            cfg = load_config(cfg_path)
            assert cfg.input.device == "/dev/input/custom"
            assert cfg.audio.sample_rate == 48000
            assert cfg.audio.channels == 2
            assert cfg.model.name == "small"
            assert cfg.model.device == "cuda"
            assert cfg.transcription.language == "fr"
            assert cfg.transcription.noise_reduction is True
        finally:
            cfg_path.unlink()
