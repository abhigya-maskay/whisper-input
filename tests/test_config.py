"""Tests for config module."""

import pytest


class TestConfig:
    """Tests for configuration loading and validation."""

    @pytest.mark.skip(reason="Implementation pending")
    def test_load_from_toml(self):
        """Test loading configuration from dictation.toml.

        TODO: Create test config file, validate parsing
        """
        pass

    @pytest.mark.skip(reason="Implementation pending")
    def test_config_validation(self):
        """Test configuration validation.

        TODO: Test invalid device paths, missing fields, etc.
        """
        pass

    @pytest.mark.skip(reason="Implementation pending")
    def test_config_defaults(self):
        """Test configuration defaults.

        TODO: Verify sensible defaults for audio, model, transcription
        """
        pass
