"""Async button listener using evdev."""

import asyncio
import logging
from dataclasses import dataclass
from typing import AsyncIterator

logger = logging.getLogger(__name__)


@dataclass
class ButtonEvent:
    """Button press/release event."""

    pressed: bool
    timestamp: float


class ButtonListener:
    """Listens to input device events and emits debounced button events.

    Wraps evdev.InputDevice for asyncio integration.
    """

    def __init__(self, device_path: str, key_code: str, debounce_ms: int = 50):
        """Initialize button listener.

        Args:
            device_path: Path to /dev/input/by-id/* device
            key_code: Symbolic key name (e.g., BTN_EXTRA)
            debounce_ms: Debounce interval in milliseconds
        """
        self.device_path = device_path
        self.key_code = key_code
        self.debounce_ms = debounce_ms
        logger.info("ButtonListener initialized for %s (%s)", device_path, key_code)

    async def listen(self) -> AsyncIterator[ButtonEvent]:
        """Emit debounced button events from device.

        Yields:
            ButtonEvent: Press/release events

        TODO: Open evdev.InputDevice, read events, apply debouncing, yield events
        """
        raise NotImplementedError("listen() not yet implemented")

    @staticmethod
    def list_devices() -> dict[str, str]:
        """List available input devices.

        Returns:
            Dict mapping device paths to descriptions

        TODO: Enumerate /dev/input/by-id and return available devices
        """
        raise NotImplementedError("list_devices() not yet implemented")
