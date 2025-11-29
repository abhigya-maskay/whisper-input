"""Async button listener using evdev."""

import asyncio
import logging
import time
from dataclasses import dataclass
from pathlib import Path
from typing import AsyncIterator, Sequence

from evdev import InputDevice, ecodes

from .config import InputConfig

logger = logging.getLogger(__name__)


@dataclass
class ButtonEvent:
    """Button press/release event tagged with originating key."""

    key_code: str
    pressed: bool
    timestamp: float


class ButtonListener:
    """Listens to input device events and emits debounced button events.

    Wraps evdev.InputDevice for asyncio integration.
    Supports monitoring multiple devices simultaneously.
    """

    def __init__(
        self,
        device_path: str | Sequence[str],
        key_code: str | Sequence[str],
        debounce_ms: int = 50,
    ):
        """Initialize button listener.

        Args:
            device_path: Path(s) to /dev/input/by-id/* device(s)
            key_code: Symbolic key name(s) (e.g., BTN_EXTRA)
            debounce_ms: Debounce interval in milliseconds
        """
        # Normalize device paths
        if isinstance(device_path, str):
            device_paths: tuple[str, ...] = (device_path,)
        else:
            device_paths = tuple(device_path)
            if not device_paths:
                raise ValueError("At least one device path must be provided")

        self.device_paths = device_paths
        self.device_path = device_paths[0]

        # Normalize key codes
        if isinstance(key_code, str):
            key_codes: tuple[str, ...] = (key_code,)
        else:
            key_codes = tuple(key_code)
            if not key_codes:
                raise ValueError("At least one key code must be provided")

        self.key_codes = key_codes
        self.key_code = key_codes[0]
        self.debounce_ms = debounce_ms
        self._devices: list[InputDevice] = []
        self._device: InputDevice | None = None
        self._last_event_time: dict[str, float] = {}
        self._last_pressed_state: dict[str, bool | None] = {}
        logger.info(
            "ButtonListener initialized for %d device(s) with key codes: %s",
            len(self.device_paths),
            ", ".join(self.key_codes),
        )

    @classmethod
    def from_config(cls, config: InputConfig, debounce_ms: int = 50) -> "ButtonListener":
        """Create ButtonListener from configuration.

        Args:
            config: InputConfig containing device(s) and key_code(s)
            debounce_ms: Debounce interval in milliseconds

        Returns:
            Configured ButtonListener instance
        """
        return cls(
            device_path=config.devices,
            key_code=config.key_codes,
            debounce_ms=debounce_ms,
        )

    async def __aenter__(self) -> "ButtonListener":
        """Async context manager entry.

        Returns:
            Self for context manager protocol

        Raises:
            RuntimeError: If any device cannot be opened
        """
        self._devices = []
        self._device = None
        for dev_path in self.device_paths:
            try:
                device = InputDevice(dev_path)
                self._devices.append(device)
                logger.debug("Opened input device: %s", dev_path)
            except (OSError, PermissionError) as e:
                # Close any already opened devices
                for dev in self._devices:
                    try:
                        dev.close()
                    except Exception:
                        pass
                self._devices = []
                self._device = None
                raise RuntimeError(f"Cannot open input device {dev_path}: {e}") from e
        if self._devices:
            self._device = self._devices[0]
        self._last_event_time.clear()
        self._last_pressed_state.clear()
        return self

    async def __aexit__(self, exc_type, exc_val, exc_tb) -> None:
        """Async context manager exit.

        Ensures all devices are properly closed.
        """
        for device in self._devices:
            try:
                device.close()
                logger.debug("Closed input device: %s", device.path)
            except Exception as e:
                logger.warning("Error closing device %s: %s", device.path, e)
        self._devices = []
        self._device = None

    async def _read_device_events(
        self,
        device: InputDevice,
        queue: asyncio.Queue[tuple[str, object]],
    ) -> None:
        """Read events from a single device and put them in the queue.

        Args:
            device: Input device to read from
            queue: Queue to put events into

        Raises:
            RuntimeError: If device access fails
        """
        invalid_codes: list[str] = []
        key_code_values: dict[int, str] = {}
        for code in self.key_codes:
            key_code_val = getattr(ecodes, code, None)
            if key_code_val is None:
                invalid_codes.append(code)
            else:
                key_code_values[key_code_val] = code

        try:
            if invalid_codes:
                if len(invalid_codes) == 1:
                    message = f"Invalid key code: {invalid_codes[0]}"
                else:
                    invalid_list = ", ".join(invalid_codes)
                    message = f"Invalid key codes: {invalid_list}"
                await queue.put(("error", RuntimeError(message)))
                return

            # Build key code lookup table
            if not key_code_values:
                logger.warning("No valid key codes for device %s", device.path)
                return

            logger.debug(
                "Listening on %s for key events: %s",
                device.path,
                ", ".join(f"{name} ({value})" for value, name in key_code_values.items()),
            )

            async for event in device.async_read_loop():
                if event.type == ecodes.EV_KEY and event.code in key_code_values:
                    # event.value: 0 = release, 1 = press, 2 = repeat (ignore)
                    if event.value in (0, 1):
                        pressed = event.value == 1
                        key_name = key_code_values[event.code]
                        logger.debug(
                            "Raw key event from %s: %s %s (value=%d)",
                            device.path,
                            key_name,
                            "pressed" if pressed else "released",
                            event.value,
                        )
                        await queue.put(("event", (key_name, pressed)))
        except (OSError, PermissionError) as e:
            logger.error("Device access failed for %s: %s", device.path, e)
            await queue.put(("error", RuntimeError(f"Device access failed for {device.path}: {e}")))
        except asyncio.CancelledError:
            logger.debug("Device reader cancelled for %s", device.path)
            raise
        except Exception as e:
            logger.error("Unexpected error reading %s: %s", device.path, e)
            await queue.put(("error", RuntimeError(f"Unexpected device error for {device.path}: {e}")))
        finally:
            try:
                await queue.put(("done", None))
            except asyncio.CancelledError:
                raise

    async def _iter_key_events(self) -> AsyncIterator[tuple[str, bool]]:
        """Iterate over key events from all configured devices.

        Yields:
            tuple[str, bool]: (key_code, True) for press, (key_code, False) for release

        Raises:
            RuntimeError: If device access fails
        """
        if not self._devices:
            raise RuntimeError("No devices opened. Use ButtonListener as async context manager.")

        # Create a queue to collect events from all devices
        queue: asyncio.Queue[tuple[str, bool | None | Exception]] = asyncio.Queue()

        # Start a task for each device
        tasks = []
        for device in self._devices:
            task = asyncio.create_task(self._read_device_events(device, queue))
            tasks.append(task)

        active_readers = len(tasks)

        try:
            # Yield events from the queue as they arrive
            while active_readers > 0:
                message_type, payload = await queue.get()

                if message_type == "event":
                    key_code, pressed = payload
                    assert isinstance(key_code, str)
                    assert isinstance(pressed, bool)
                    yield key_code, pressed
                elif message_type == "done":
                    active_readers -= 1
                elif message_type == "error":
                    assert isinstance(payload, Exception)
                    raise payload
        finally:
            logger.debug("Cancelling all device reader tasks")
            for task in tasks:
                task.cancel()
            await asyncio.gather(*tasks, return_exceptions=True)

    async def listen(self) -> AsyncIterator[ButtonEvent]:
        """Emit debounced button events from device.

        Yields:
            ButtonEvent: Debounced press/release events

        Raises:
            RuntimeError: If device access fails
        """
        logger.debug("Starting button listener with %dms debounce", self.debounce_ms)

        try:
            async for key_code, pressed in self._iter_key_events():
                current_time = time.time()

                # Apply debouncing
                last_time = self._last_event_time.get(key_code, 0.0)
                last_state = self._last_pressed_state.get(key_code)
                time_since_last = (current_time - last_time) * 1000

                if (
                    last_state is None
                    or time_since_last >= self.debounce_ms
                    or pressed != last_state
                ):
                    event = ButtonEvent(
                        key_code=key_code,
                        pressed=pressed,
                        timestamp=current_time,
                    )

                    logger.debug(
                        "Emitting button event: %s %s at %.3f",
                        key_code,
                        "pressed" if pressed else "released",
                        current_time,
                    )

                    self._last_event_time[key_code] = current_time
                    self._last_pressed_state[key_code] = pressed

                    yield event
                else:
                    logger.debug(
                        "Debouncing event: %s %s at %.3f (last: %.3f, %.1fms ago)",
                        key_code,
                        "pressed" if pressed else "released",
                        current_time,
                        last_time,
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
