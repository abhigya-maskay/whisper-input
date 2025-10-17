"""Tests for main CLI module."""

import pytest


class TestMainCLI:
    """Tests for Typer CLI commands."""

    @pytest.mark.skip(reason="Implementation pending")
    def test_run_command(self):
        """Test run command initialization.

        TODO: Mock config loading, button listener, orchestrator
        """
        pass

    @pytest.mark.skip(reason="Implementation pending")
    def test_list_inputs_command(self):
        """Test list-inputs device enumeration.

        TODO: Mock evdev device listing
        """
        pass

    @pytest.mark.skip(reason="Implementation pending")
    def test_list_audio_command(self):
        """Test list-audio device enumeration.

        TODO: Mock sounddevice query
        """
        pass

    @pytest.mark.skip(reason="Implementation pending")
    def test_dry_run_command(self):
        """Test dry-run mode without injection.

        TODO: Mock orchestrator in dry-run mode
        """
        pass
