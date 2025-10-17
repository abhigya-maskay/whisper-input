"""Tests for main CLI module."""

import json
from pathlib import Path
from unittest.mock import MagicMock, patch

import pytest
from typer.testing import CliRunner

from dictation_app.main import app

runner = CliRunner()


class TestListInputsCommand:
    """Tests for list-inputs command."""

    @patch("dictation_app.main.discover_input_devices")
    def test_list_inputs_table_output(self, mock_discover):
        """Test list-inputs command with table output."""
        mock_discover.return_value = [
            {
                "path": "/dev/input/by-id/test-device",
                "name": "Test Device",
                "capabilities": [1, 2],
            }
        ]

        result = runner.invoke(app, ["list-inputs"])
        assert result.exit_code == 0
        assert "Available input devices:" in result.stdout
        assert "/dev/input/by-id/test-device" in result.stdout
        assert "Test Device" in result.stdout

    @patch("dictation_app.main.discover_input_devices")
    def test_list_inputs_json_output(self, mock_discover):
        """Test list-inputs command with JSON output."""
        mock_discover.return_value = [
            {
                "path": "/dev/input/by-id/test-device",
                "name": "Test Device",
                "capabilities": [1, 2],
            }
        ]

        result = runner.invoke(app, ["list-inputs", "--json"])
        assert result.exit_code == 0
        data = json.loads(result.stdout)
        assert isinstance(data, list)
        assert data[0]["path"] == "/dev/input/by-id/test-device"

    @patch("dictation_app.main.discover_input_devices")
    def test_list_inputs_no_devices(self, mock_discover):
        """Test list-inputs when no devices found."""
        mock_discover.return_value = []

        result = runner.invoke(app, ["list-inputs"])
        assert result.exit_code == 0


class TestListAudioCommand:
    """Tests for list-audio command."""

    @patch("dictation_app.main.discover_audio_devices")
    def test_list_audio_table_output(self, mock_discover):
        """Test list-audio command with table output."""
        mock_discover.return_value = [
            {
                "index": 0,
                "name": "Microphone",
                "channels": 2,
                "sample_rate": 48000,
            }
        ]

        result = runner.invoke(app, ["list-audio"])
        assert result.exit_code == 0
        assert "Available audio devices:" in result.stdout
        assert "Microphone" in result.stdout
        assert "2ch" in result.stdout
        assert "48000Hz" in result.stdout

    @patch("dictation_app.main.discover_audio_devices")
    def test_list_audio_json_output(self, mock_discover):
        """Test list-audio command with JSON output."""
        mock_discover.return_value = [
            {
                "index": 0,
                "name": "Microphone",
                "channels": 2,
                "sample_rate": 48000,
            }
        ]

        result = runner.invoke(app, ["list-audio", "--json"])
        assert result.exit_code == 0
        data = json.loads(result.stdout)
        assert isinstance(data, list)
        assert data[0]["name"] == "Microphone"

    @patch("dictation_app.main.discover_audio_devices")
    def test_list_audio_no_devices(self, mock_discover):
        """Test list-audio when no devices found."""
        mock_discover.return_value = []

        result = runner.invoke(app, ["list-audio"])
        assert result.exit_code == 0


class TestRunCommand:
    """Tests for run command."""

    @patch("dictation_app.main.load_config")
    def test_run_command_success(self, mock_load_config):
        """Test run command with valid config."""
        mock_cfg = MagicMock()
        mock_cfg.validate = MagicMock()
        mock_load_config.return_value = mock_cfg

        result = runner.invoke(app, ["run"])
        assert result.exit_code == 0

    @patch("dictation_app.main.load_config")
    def test_run_command_with_config_path(self, mock_load_config):
        """Test run command with explicit config path."""
        mock_cfg = MagicMock()
        mock_cfg.validate = MagicMock()
        mock_load_config.return_value = mock_cfg

        result = runner.invoke(app, ["run", "--config", "/path/to/config.toml"])
        assert result.exit_code == 0
        mock_load_config.assert_called_once()

    @patch("dictation_app.main.load_config")
    def test_run_command_config_error(self, mock_load_config):
        """Test run command with config loading error."""
        from dictation_app.config import ConfigError

        mock_load_config.side_effect = ConfigError("Invalid config")

        result = runner.invoke(app, ["run"])
        assert result.exit_code == 1

    @patch("dictation_app.main.load_config")
    def test_run_command_validation_error(self, mock_load_config):
        """Test run command with validation error."""
        from dictation_app.config import ConfigError

        mock_cfg = MagicMock()
        mock_cfg.validate.side_effect = ConfigError("Device not found")
        mock_load_config.return_value = mock_cfg

        result = runner.invoke(app, ["run"])
        assert result.exit_code == 1


class TestDryRunCommand:
    """Tests for dry-run command."""

    @patch("dictation_app.main.load_config")
    def test_dry_run_command_success(self, mock_load_config):
        """Test dry-run command with valid config."""
        mock_cfg = MagicMock()
        mock_cfg.validate = MagicMock()
        mock_load_config.return_value = mock_cfg

        result = runner.invoke(app, ["dry-run"])
        assert result.exit_code == 0

    @patch("dictation_app.main.load_config")
    def test_dry_run_command_with_config_path(self, mock_load_config):
        """Test dry-run command with explicit config path."""
        mock_cfg = MagicMock()
        mock_cfg.validate = MagicMock()
        mock_load_config.return_value = mock_cfg

        result = runner.invoke(app, ["dry-run", "--config", "/path/to/config.toml"])
        assert result.exit_code == 0
        mock_load_config.assert_called_once()

    @patch("dictation_app.main.load_config")
    def test_dry_run_command_config_error(self, mock_load_config):
        """Test dry-run command with config error."""
        from dictation_app.config import ConfigError

        mock_load_config.side_effect = ConfigError("Invalid config")

        result = runner.invoke(app, ["dry-run"])
        assert result.exit_code == 1
