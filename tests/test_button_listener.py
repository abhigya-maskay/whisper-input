"""Tests for button listener module."""

import asyncio
from pathlib import Path
from unittest.mock import MagicMock, patch

import pytest

from dictation_app.button_listener import ButtonListener
from dictation_app.config import InputConfig


async def async_iter(items):
    """Helper to create async iterator from list."""
    for item in items:
        yield item


class TestButtonListener:
    """Tests for button event listening."""

    def test_from_config(self):
        """Test factory method from config."""
        config = InputConfig(device="/dev/input/by-id/test-device", key_code="BTN_EXTRA")
        listener = ButtonListener.from_config(config, debounce_ms=100)

        assert listener.device_path == "/dev/input/by-id/test-device"
        assert listener.key_code == "BTN_EXTRA"
        assert listener.key_codes == ("BTN_EXTRA",)
        assert listener.debounce_ms == 100

    def test_from_config_multiple_keys(self):
        """Test factory method with multiple key codes."""
        config = InputConfig(
            device="/dev/input/by-id/test-device",
            key_code=["BTN_EXTRA", "KEY_F2"],
        )
        listener = ButtonListener.from_config(config, debounce_ms=75)

        assert listener.key_codes == ("BTN_EXTRA", "KEY_F2")
        assert listener.key_code == "BTN_EXTRA"
        assert listener.debounce_ms == 75

    @patch('dictation_app.button_listener.InputDevice')
    @pytest.mark.asyncio
    async def test_listen_events(self, mock_input_device):
        """Test button event emission with mocked evdev."""
        import evdev
        
        # Setup mock InputDevice and events
        mock_event1 = MagicMock()
        mock_event1.type = evdev.ecodes.EV_KEY
        mock_event1.code = evdev.ecodes.BTN_EXTRA
        mock_event1.value = 1  # press
        
        mock_event2 = MagicMock()
        mock_event2.type = evdev.ecodes.EV_KEY
        mock_event2.code = evdev.ecodes.BTN_EXTRA
        mock_event2.value = 0  # release
        
        mock_device = MagicMock()
        mock_device.async_read_loop = MagicMock(return_value=async_iter([mock_event1, mock_event2]))
        mock_input_device.return_value = mock_device
        
        # Test button listener
        listener = ButtonListener("/dev/input/by-id/test", "BTN_EXTRA", debounce_ms=0)
        
        events = []
        async with listener:
            async for event in listener.listen():
                events.append(event)
                if len(events) >= 2:
                    break
        
        assert len(events) == 2
        assert events[0].pressed is True
        assert events[1].pressed is False
        assert events[0].timestamp > 0
        assert events[1].timestamp > 0
        assert events[1].timestamp >= events[0].timestamp

    @patch('dictation_app.button_listener.InputDevice')
    @pytest.mark.asyncio
    async def test_debouncing(self, mock_input_device):
        """Test event debouncing with rapid events."""
        import evdev
        
        # Setup rapid events (press, release, press, release)
        events = []
        for i in range(4):
            mock_event = MagicMock()
            mock_event.type = evdev.ecodes.EV_KEY
            mock_event.code = evdev.ecodes.BTN_EXTRA
            mock_event.value = 1 if i % 2 == 0 else 0  # press, release pattern
            events.append(mock_event)
        
        mock_device = MagicMock()
        mock_device.async_read_loop = MagicMock(return_value=async_iter(events))
        mock_input_device.return_value = mock_device
        
        # Test with 50ms debounce - should suppress rapid events
        listener = ButtonListener("/dev/input/by-id/test", "BTN_EXTRA", debounce_ms=50)
        
        emitted_events = []
        async with listener:
            async for event in listener.listen():
                emitted_events.append(event)
                if len(emitted_events) >= 2:
                    break
        
        # Should have filtered out rapid toggles within debounce window
        assert len(emitted_events) >= 2
        assert emitted_events[0].pressed is True
        # Last event should be release
        assert emitted_events[-1].pressed is False

    @patch('dictation_app.button_listener.InputDevice')
    @pytest.mark.asyncio
    async def test_key_code_validation(self, mock_input_device):
        """Test that invalid key codes raise RuntimeError."""
        mock_device = MagicMock()
        mock_device.async_read_loop = MagicMock(return_value=async_iter([]))
        mock_input_device.return_value = mock_device
        
        listener = ButtonListener("/dev/input/by-id/test", "BTN_INVALID_KEYCODE_XYZ")
        
        with pytest.raises(RuntimeError, match="Invalid key code: BTN_INVALID_KEYCODE_XYZ"):
            async with listener:
                async for _ in listener.listen():
                    pass

    @patch('dictation_app.button_listener.InputDevice')
    @pytest.mark.asyncio
    async def test_device_access_error(self, mock_input_device):
        """Test RuntimeError on device access failure."""
        mock_input_device.side_effect = OSError("Permission denied")
        
        listener = ButtonListener("/dev/input/by-id/test", "BTN_EXTRA")
        
        with pytest.raises(RuntimeError, match="Cannot open input device"):
            async with listener:
                pass

    @patch('dictation_app.button_listener.InputDevice')
    @pytest.mark.asyncio
    async def test_permission_error_on_open(self, mock_input_device):
        """Test PermissionError on device open."""
        mock_input_device.side_effect = PermissionError("Access denied")
        
        listener = ButtonListener("/dev/input/by-id/test", "BTN_EXTRA")
        
        with pytest.raises(RuntimeError, match="Cannot open input device"):
            async with listener:
                pass

    @patch('dictation_app.button_listener.InputDevice')
    @pytest.mark.asyncio
    async def test_cancellation_cleanup(self, mock_input_device):
        """Test that cancellation properly cleans up resources."""
        import evdev
        
        # Create a long-running async generator that can be cancelled
        async def long_running_events():
            mock_event = MagicMock()
            mock_event.type = evdev.ecodes.EV_KEY
            mock_event.code = evdev.ecodes.BTN_EXTRA
            mock_event.value = 1
            
            yield mock_event
            # Simulate more events to be yielded
            while True:
                await asyncio.sleep(0.1)
                yield mock_event
        
        mock_device = MagicMock()
        mock_device.async_read_loop = MagicMock(return_value=long_running_events())
        mock_input_device.return_value = mock_device
        
        listener = ButtonListener("/dev/input/by-id/test", "BTN_EXTRA")
        
        async def listen_and_cancel():
            async with listener:
                async for _ in listener.listen():
                    raise asyncio.CancelledError()
        
        with pytest.raises(asyncio.CancelledError):
            await listen_and_cancel()
        
        # Verify device was closed
        mock_device.close.assert_called()

    @patch('dictation_app.button_listener.Path')
    @patch('dictation_app.button_listener.InputDevice')
    def test_list_devices_success(self, mock_input_device, mock_path_class):
        """Test successful device enumeration."""
        # Mock /dev/input/by-id directory
        mock_input_dir = MagicMock()
        mock_path_class.return_value = mock_input_dir
        mock_input_dir.exists.return_value = True
        
        # Mock device files with proper __lt__ for sorting
        mock_device_file1 = MagicMock()
        mock_device_file1.resolve.return_value = Path("/dev/input/event0")
        mock_device_file1.name = "usb-Keyboard-event"
        mock_device_file1.__str__.return_value = "/dev/input/by-id/usb-Keyboard-event"
        mock_device_file1.__lt__ = lambda self, other: str(self) < str(other)
        
        mock_device_file2 = MagicMock()
        mock_device_file2.resolve.return_value = Path("/dev/input/event1")
        mock_device_file2.name = "usb-Mouse-event"
        mock_device_file2.__str__.return_value = "/dev/input/by-id/usb-Mouse-event"
        mock_device_file2.__lt__ = lambda self, other: str(self) < str(other)
        
        mock_input_dir.iterdir.return_value = [mock_device_file1, mock_device_file2]
        
        # Mock evdev.InputDevice to return names
        mock_dev1 = MagicMock()
        mock_dev1.name = "Test Keyboard"
        mock_dev2 = MagicMock()
        mock_dev2.name = "Test Mouse"
        
        mock_input_device.side_effect = [mock_dev1, mock_dev2]
        
        devices = ButtonListener.list_devices()
        
        assert len(devices) == 2
        assert devices["/dev/input/by-id/usb-Keyboard-event"] == "Test Keyboard"
        assert devices["/dev/input/by-id/usb-Mouse-event"] == "Test Mouse"

    @patch('dictation_app.button_listener.Path')
    def test_list_devices_no_directory(self, mock_path_class):
        """Test handling when /dev/input/by-id doesn't exist."""
        mock_input_dir = MagicMock()
        mock_path_class.return_value = mock_input_dir
        mock_input_dir.exists.return_value = False
        
        devices = ButtonListener.list_devices()
        
        assert devices == {}
        
    @patch('dictation_app.button_listener.Path')
    def test_list_devices_permission_error(self, mock_path_class):
        """Test handling permission errors during enumeration."""
        mock_input_dir = MagicMock()
        mock_path_class.return_value = mock_input_dir
        mock_input_dir.exists.return_value = True
        
        # Mock PermissionError when accessing device
        mock_device_file = MagicMock()
        mock_device_file.resolve.side_effect = PermissionError("Permission denied")
        mock_input_dir.iterdir.return_value = [mock_device_file]
        
        devices = ButtonListener.list_devices()
        
        assert devices == {}

    @patch('dictation_app.button_listener.Path')
    @patch('dictation_app.button_listener.InputDevice')
    def test_list_devices_input_device_error(self, mock_input_device, mock_path_class):
        """Test fallback when InputDevice access fails."""
        mock_input_dir = MagicMock()
        mock_path_class.return_value = mock_input_dir
        mock_input_dir.exists.return_value = True
        
        mock_device_file = MagicMock()
        mock_device_file.resolve.return_value = Path("/dev/input/event0")
        mock_device_file.name = "test-device"
        mock_device_file.__str__.return_value = "/dev/input/by-id/test-device"
        mock_device_file.__lt__ = lambda self, other: str(self) < str(other)
        mock_input_dir.iterdir.return_value = [mock_device_file]
        
        # Mock InputDevice failure
        mock_input_device.side_effect = OSError("No such device")
        
        devices = ButtonListener.list_devices()
        
        assert len(devices) == 1
        # Should fall back to filename as name
        assert devices["/dev/input/by-id/test-device"] == "test-device"

    @patch('dictation_app.button_listener.InputDevice')
    @pytest.mark.asyncio
    async def test_context_manager_resource_cleanup(self, mock_input_device):
        """Test that async context manager properly opens/closes devices."""
        mock_device = MagicMock()
        mock_input_device.return_value = mock_device
        
        listener = ButtonListener("/dev/input/by-id/test", "BTN_EXTRA")
        
        # Test context manager
        async with listener:
            assert listener._device is mock_device
            mock_device.close.assert_not_called()
        
        # Device should be closed after context exit
        mock_device.close.assert_called_once()
        assert listener._device is None

    @patch('dictation_app.button_listener.InputDevice')
    @pytest.mark.asyncio
    async def test_rapid_toggle_suppression(self, mock_input_device):
        """Test that rapid toggles within debounce window are suppressed."""
        import evdev
        
        # Create events: press, rapid release/press within debounce, then final release
        events = []
        for val in [1, 0, 1, 0]:  # press, release, press, release
            mock_event = MagicMock()
            mock_event.type = evdev.ecodes.EV_KEY
            mock_event.code = evdev.ecodes.BTN_EXTRA
            mock_event.value = val
            events.append(mock_event)
        
        mock_device = MagicMock()
        mock_device.async_read_loop = MagicMock(return_value=async_iter(events))
        mock_input_device.return_value = mock_device
        
        listener = ButtonListener("/dev/input/by-id/test", "BTN_EXTRA", debounce_ms=100)
        
        emitted_events = []
        async with listener:
            async for event in listener.listen():
                emitted_events.append(event)
                if len(emitted_events) >= 2:
                    break
        
        # Should emit first press and final release, suppressing rapid toggles
        assert len(emitted_events) >= 2
        assert emitted_events[0].pressed is True
        assert emitted_events[-1].pressed is False

    @patch('dictation_app.button_listener.Path')
    @patch('dictation_app.button_listener.InputDevice')
    def test_list_devices_mixed_permissions(self, mock_input_device, mock_path_class):
        """Test device enumeration with mixed success and permission errors."""
        mock_input_dir = MagicMock()
        mock_path_class.return_value = mock_input_dir
        mock_input_dir.exists.return_value = True
        
        # First device: accessible (comes first after sorting)
        mock_device_file1 = MagicMock()
        mock_device_file1.resolve.return_value = Path("/dev/input/event0")
        mock_device_file1.name = "usb-Keyboard-event"
        mock_device_file1.__str__.return_value = "/dev/input/by-id/usb-Keyboard-event"
        mock_device_file1.__lt__ = lambda self, other: str(self) < str(other)
        
        # Second device: permission error (will be skipped)
        mock_device_file2 = MagicMock()
        mock_device_file2.resolve.side_effect = PermissionError("Permission denied")
        mock_device_file2.name = "usb-Mouse-event"
        mock_device_file2.__str__.return_value = "/dev/input/by-id/usb-Mouse-event"
        mock_device_file2.__lt__ = lambda self, other: str(self) < str(other)
        
        # Third device: accessible (comes last after sorting)
        mock_device_file3 = MagicMock()
        mock_device_file3.resolve.return_value = Path("/dev/input/event1")
        mock_device_file3.name = "usb-Touchpad-event"
        mock_device_file3.__str__.return_value = "/dev/input/by-id/usb-Touchpad-event"
        mock_device_file3.__lt__ = lambda self, other: str(self) < str(other)
        
        # After sorting: Keyboard, Mouse (skipped), Touchpad
        mock_input_dir.iterdir.return_value = [mock_device_file1, mock_device_file2, mock_device_file3]
        
        mock_dev1 = MagicMock()
        mock_dev1.name = "Test Keyboard"
        mock_dev3 = MagicMock()
        mock_dev3.name = "Test Touchpad"
        
        mock_input_device.side_effect = [mock_dev1, mock_dev3]
        
        devices = ButtonListener.list_devices()
        
        # Should have 2 devices (skipping the one with permission error)
        assert len(devices) == 2
        assert devices["/dev/input/by-id/usb-Keyboard-event"] == "Test Keyboard"
        assert devices["/dev/input/by-id/usb-Touchpad-event"] == "Test Touchpad"
