"""Tests for button listener module."""

import pytest


class TestButtonListener:
    """Tests for button event listening."""

    @pytest.mark.skip(reason="Implementation pending")
    @pytest.mark.asyncio
    async def test_listen_events(self):
        """Test button event emission.

        TODO: Mock evdev.InputDevice, verify press/release events
        """
        pass

    @pytest.mark.skip(reason="Implementation pending")
    @pytest.mark.asyncio
    async def test_debouncing(self):
        """Test event debouncing.

        TODO: Simulate rapid events, verify debounce delay
        """
        pass

    @pytest.mark.skip(reason="Implementation pending")
    def test_list_devices(self):
        """Test device enumeration.

        TODO: Mock /dev/input/by-id and verify device listing
        """
        pass
