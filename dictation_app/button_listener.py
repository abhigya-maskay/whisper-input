"""Async button listener using evdev."""

import asyncio
import logging
import time
from contextlib import asynccontextmanager
from dataclasses import dataclass
from pathlib import Path
from typing import AsyncIterator, Sequence

from evdev import InputDevice, ecodes

from .config import InputConfig

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

    def __init__(
        self,
        device_path: str,
        key_code: str | Sequence[str],
        debounce_ms: int = 50,
    ):
        """Initialize button listener.

        Args:
            device_path: Path to /dev/input/by-id/* device
            key_code: Symbolic key name(s) (e.g., BTN_EXTRA)
            debounce_ms: Debounce interval in milliseconds
        """
        self.device_path = device_path
        if isinstance(key_code, str):
            key_codes: tuple[str, ...] = (key_code,)
        else:
            key_codes = tuple(key_code)
            if not key_codes:
                raise ValueError("At least one key code must be provided")

        self.key_codes = key_codes
        self.key_code = key_codes[0]
        self.debounce_ms = debounce_ms
        self._device: InputDevice | None = None
        self._last_event_time: float = 0.0
        self._last_pressed_state: bool | None = None
        logger.info(
            "ButtonListener initialized for %s (%s)",
            device_path,
            ", ".join(self.key_codes),
        )

    @classmethod
    def from_config(cls, config: InputConfig, debounce_ms: int = 50) -> "ButtonListener":
        """Create ButtonListener from configuration.

        Args:
            config: InputConfig containing device and key_code(s)
            debounce_ms: Debounce interval in milliseconds

        Returns:
            Configured ButtonListener instance
        """
        return cls(
            device_path=config.device,
            key_code=config.key_codes,
            debounce_ms=debounce_ms,
        )

    async def __aenter__(self) -> "ButtonListener":
        """Async context manager entry.

        Returns:
            Self for context manager protocol

        Raises:
            RuntimeError: If device cannot be opened
        """
        try:
            self._device = InputDevice(self.device_path)
            logger.debug("Opened input device: %s", self.device_path)
            return self
        except (OSError, PermissionError) as e:
            raise RuntimeError(f"Cannot open input device {self.device_path}: {e}") from e

    async def __aexit__(self, exc_type, exc_val, exc_tb) -> None:
        """Async context manager exit.

        Ensures device is properly closed.
        """
        if self._device:
            try:
                self._device.close()
                logger.debug("Closed input device: %s", self.device_path)
            except Exception as e:
                logger.warning("Error closing device %s: %s", self.device_path, e)
            finally:
                self._device = None

    @asynccontextmanager
    async def _ensure_device_open(self):
        """Ensure device is open for internal operations.

        Yields:
            Open InputDevice instance
        """
        if not self._device:
            async with self:
                yield self._device
        else:
            yield self._device

    async def _iter_key_events(self) -> AsyncIterator[bool]:
        """Iterate over key events for the configured key code(s).

        Yields:
            bool: True for key press, False for key release

        Raises:
            RuntimeError: If device access fails
        """
        try:
            async with self._ensure_device_open() as device:
                key_code_values: dict[int, str] = {}
                for code in self.key_codes:
                    key_code_val = getattr(ecodes, code, None)
                    if key_code_val is None:
                        raise RuntimeError(f"Invalid key code: {code}")
                    key_code_values[key_code_val] = code

                logger.debug(
                    "Listening for key events: %s",
                    ", ".join(f"{name} ({value})" for value, name in key_code_values.items()),
                )

                async for event in device.async_read_loop():
                    if event.type == ecodes.EV_KEY and event.code in key_code_values:
                        # event.value: 0 = release, 1 = press, 2 = repeat (ignore)
                        if event.value in (0, 1):
                            pressed = event.value == 1
                            key_name = key_code_values[event.code]
                            logger.debug(
                                "Raw key event: %s %s (value=%d)",
                                key_name,
                                "pressed" if pressed else "released",
                                event.value,
                            )
                            yield pressed
        except (OSError, PermissionError) as e:
            raise RuntimeError(f"Device access failed: {e}") from e

    async def listen(self) -> AsyncIterator[ButtonEvent]:
        """Emit debounced button events from device.

        Yields:
            ButtonEvent: Debounced press/release events

        Raises:
            RuntimeError: If device access fails
        """
        logger.debug("Starting button listener with %dms debounce", self.debounce_ms)

        try:
            async for pressed in self._iter_key_events():
                current_time = time.time()

                # Apply debouncing
                time_since_last = (current_time - self._last_event_time) * 1000

                if (
                    self._last_pressed_state is None
                    or time_since_last >= self.debounce_ms
                    or pressed != self._last_pressed_state
                ):
                    event = ButtonEvent(pressed=pressed, timestamp=current_time)

                    logger.debug(
                        "Emitting button event: %s at %.3f",
                        "pressed" if pressed else "released",
                        current_time,
                    )

                    self._last_event_time = current_time
                    self._last_pressed_state = pressed

                    yield event
                else:
                    logger.debug(
                        "Debouncing event: %s at %.3f (last: %.3f, %.1fms ago)",
                        "pressed" if pressed else "released",
                        current_time,
                        self._last_event_time,
                        time_since_last,
                    )
        except asyncio.CancelledError:
            logger.debug("Button listener cancelled")
            raise
        except Exception as e:
            logger.error("Button listener error: %s", e)
            raise RuntimeError(f"Button listener failed: {e}") from e

    @staticmethod
    def list_devices() -> dict[str, str]:
        """List available input devices.

        Returns:
            Dict mapping device paths to friendly names
            Empty dict if no devices found or access denied
        """
        devices: dict[str, str] = {}
        input_dir = Path("/dev/input/by-id")

        try:
            if not input_dir.exists():
                logger.warning("/dev/input/by-id directory not found")
                return devices

            for device_path in sorted(input_dir.iterdir()):
                try:
                    # Resolve symlink to actual device
                    resolved_path = device_path.resolve()

                    # Get device name from evdev
                    try:
                        device = InputDevice(str(resolved_path))
                        name = device.name
                    except (OSError, PermissionError):
                        # Fallback to filename if cannot access device
                        name = device_path.name
                        logger.debug(
                            "Cannot access device %s for name, using filename",
                            resolved_path,
                        )

                    devices[str(device_path)] = name
                    logger.debug("Found device: %s -> %s", device_path, name)

                except (OSError, PermissionError) as e:
                    logger.debug("Cannot access device %s: %s", device_path, e)
                    continue

        except Exception as e:
            logger.warning("Error enumerating input devices: %s", e)

        logger.info("Found %d input devices", len(devices))
        return devices
