"""Tests for text injector module."""

import asyncio
from pathlib import Path
from unittest.mock import AsyncMock, MagicMock, patch

import pytest

from dictation_app.config import InjectorConfig
from dictation_app.injector import (
    CommandNotFoundError,
    Injector,
    InjectionError,
    TimeoutError,
)


class TestInjectorInitialization:
    """Tests for Injector initialization and binary validation."""

    @patch("dictation_app.injector.shutil.which")
    def test_init_wtype_backend_success(self, mock_which):
        """Test initialization with wtype backend when binary is available."""
        mock_which.return_value = "/usr/bin/wtype"

        config = InjectorConfig(backend="wtype", clipboard_mode=False)
        injector = Injector(config)

        assert injector.backend == "wtype"
        assert injector.clipboard_mode is False
        mock_which.assert_called_with("wtype")

    @patch("dictation_app.injector.shutil.which")
    def test_init_ydotool_backend_success(self, mock_which):
        """Test initialization with ydotool backend when binary is available."""
        mock_which.return_value = "/usr/bin/ydotool"

        config = InjectorConfig(backend="ydotool", clipboard_mode=False)
        injector = Injector(config)

        assert injector.backend == "ydotool"
        mock_which.assert_called_with("ydotool")

    @patch("dictation_app.injector.shutil.which")
    def test_init_xdotool_backend_success(self, mock_which):
        """Test initialization with xdotool backend when binary is available."""
        mock_which.return_value = "/usr/bin/xdotool"

        config = InjectorConfig(backend="xdotool", clipboard_mode=False)
        injector = Injector(config)

        assert injector.backend == "xdotool"
        mock_which.assert_called_with("xdotool")

    @patch("dictation_app.injector.shutil.which")
    def test_init_missing_backend_binary(self, mock_which):
        """Test initialization fails when backend binary is not found."""
        mock_which.return_value = None

        config = InjectorConfig(backend="wtype", clipboard_mode=False)

        with pytest.raises(CommandNotFoundError) as exc_info:
            Injector(config)

        assert "wtype" in str(exc_info.value)

    @patch("dictation_app.injector.shutil.which")
    def test_init_with_clipboard_mode_validates_wl_copy(self, mock_which):
        """Test initialization with clipboard_mode validates wl-copy binary."""

        def which_side_effect(binary):
            if binary == "wtype":
                return "/usr/bin/wtype"
            elif binary == "wl-copy":
                return "/usr/bin/wl-copy"
            return None

        mock_which.side_effect = which_side_effect

        config = InjectorConfig(backend="wtype", clipboard_mode=True)
        injector = Injector(config)

        assert injector.clipboard_mode is True
        assert mock_which.call_count >= 2

    @patch("dictation_app.injector.shutil.which")
    def test_init_clipboard_mode_missing_wl_copy(self, mock_which):
        """Test initialization fails when wl-copy missing but clipboard_mode=True."""

        def which_side_effect(binary):
            if binary == "wtype":
                return "/usr/bin/wtype"
            return None

        mock_which.side_effect = which_side_effect

        config = InjectorConfig(backend="wtype", clipboard_mode=True)

        with pytest.raises(CommandNotFoundError) as exc_info:
            Injector(config)

        assert "wl-copy" in str(exc_info.value)

    @patch("dictation_app.injector.shutil.which")
    def test_init_clipboard_mode_validates_xclip(self, mock_which):
        """Clipboard mode for xdotool validates xclip."""

        def which_side_effect(binary):
            if binary == "xdotool":
                return "/usr/bin/xdotool"
            if binary == "xclip":
                return "/usr/bin/xclip"
            return None

        mock_which.side_effect = which_side_effect

        config = InjectorConfig(backend="xdotool", clipboard_mode=True)
        injector = Injector(config)

        assert injector.clipboard_mode is True
        assert mock_which.call_count >= 2

    @patch("dictation_app.injector.shutil.which")
    def test_init_clipboard_mode_missing_xclip(self, mock_which):
        """Initialization fails when xclip missing for clipboard mode."""

        def which_side_effect(binary):
            if binary == "xdotool":
                return "/usr/bin/xdotool"
            return None

        mock_which.side_effect = which_side_effect

        config = InjectorConfig(backend="xdotool", clipboard_mode=True)

        with pytest.raises(CommandNotFoundError) as exc_info:
            Injector(config)

        assert "xclip" in str(exc_info.value)

    @patch("dictation_app.injector.shutil.which")
    def test_init_respects_config_settings(self, mock_which):
        """Test that Injector respects all config settings."""
        mock_which.return_value = "/usr/bin/wtype"

        config = InjectorConfig(
            backend="wtype",
            clipboard_mode=True,
            typing_delay=100,
            use_newline=True,
            timeout=20.0,
            dry_run=False,
        )
        injector = Injector(config)

        assert injector.typing_delay == 100
        assert injector.use_newline is True
        assert injector.timeout == 20.0
        assert injector.dry_run is False


class TestCommandResolution:
    """Tests for command resolution logic."""

    @patch("dictation_app.injector.shutil.which")
    def test_resolve_wtype_command_basic(self, mock_which):
        """Test basic wtype command resolution."""
        mock_which.return_value = "/usr/bin/wtype"

        config = InjectorConfig(backend="wtype", typing_delay=0)
        injector = Injector(config)

        cmd = injector._resolve_wtype_command("hello world")

        assert cmd == ["/usr/bin/wtype", "hello world"]

    @patch("dictation_app.injector.shutil.which")
    def test_resolve_wtype_command_with_delay(self, mock_which):
        """Test wtype command with typing delay."""
        mock_which.return_value = "/usr/bin/wtype"

        config = InjectorConfig(backend="wtype", typing_delay=50)
        injector = Injector(config)

        cmd = injector._resolve_wtype_command("hello")

        assert "-d" in cmd
        assert "50" in cmd

    @patch("dictation_app.injector.shutil.which")
    def test_resolve_wtype_command_with_newline(self, mock_which):
        """Test wtype command with newline appended."""
        mock_which.return_value = "/usr/bin/wtype"

        config = InjectorConfig(backend="wtype", use_newline=True)
        injector = Injector(config)

        cmd = injector._resolve_wtype_command("hello")

        assert cmd[-1] == "hello\n"

    @patch("dictation_app.injector.shutil.which")
    def test_resolve_ydotool_command_basic(self, mock_which):
        """Test basic ydotool command resolution."""
        mock_which.return_value = "/usr/bin/ydotool"

        config = InjectorConfig(backend="ydotool", typing_delay=0)
        injector = Injector(config)

        cmd = injector._resolve_ydotool_command("hello world")

        assert cmd[0] == "/usr/bin/ydotool"
        assert cmd[1] == "type"
        assert "hello world" in cmd

    @patch("dictation_app.injector.shutil.which")
    def test_resolve_ydotool_command_with_delay(self, mock_which):
        """Test ydotool command with typing delay."""
        mock_which.return_value = "/usr/bin/ydotool"

        config = InjectorConfig(backend="ydotool", typing_delay=50)
        injector = Injector(config)

        cmd = injector._resolve_ydotool_command("hello")

        assert "--delay" in cmd
        assert "50" in cmd

    @patch("dictation_app.injector.shutil.which")
    def test_resolve_ydotool_command_with_newline(self, mock_which):
        """Test ydotool command with newline appended."""
        mock_which.return_value = "/usr/bin/ydotool"

        config = InjectorConfig(backend="ydotool", use_newline=True)
        injector = Injector(config)

        cmd = injector._resolve_ydotool_command("hello")

        assert "hello\n" in cmd

    @patch("dictation_app.injector.shutil.which")
    def test_resolve_xdotool_command_basic(self, mock_which):
        """Test xdotool command with clearmodifiers flag."""
        mock_which.return_value = "/usr/bin/xdotool"

        config = InjectorConfig(backend="xdotool", typing_delay=0)
        injector = Injector(config)

        cmd = injector._resolve_xdotool_command("hello")

        assert cmd[:3] == ["/usr/bin/xdotool", "type", "--clearmodifiers"]
        assert cmd[-1] == "hello"

    @patch("dictation_app.injector.shutil.which")
    def test_resolve_xdotool_command_with_delay_and_newline(self, mock_which):
        """Test xdotool command adds delay and newline."""
        mock_which.return_value = "/usr/bin/xdotool"

        config = InjectorConfig(backend="xdotool", typing_delay=25, use_newline=True)
        injector = Injector(config)

        cmd = injector._resolve_xdotool_command("hello")

        assert "--delay" in cmd
        assert "25" in cmd
        assert cmd[-1] == "hello\n"


class TestKeyActions:
    """Tests for discrete key action helpers."""

    @patch("dictation_app.injector.shutil.which")
    def test_resolve_wtype_key_action_shift_tab(self, mock_which):
        """Shift+Tab command uses modifier sequence."""
        mock_which.return_value = "/usr/bin/wtype"

        config = InjectorConfig(backend="wtype", typing_delay=50)
        injector = Injector(config)

        cmd = injector._resolve_wtype_key_action("shift_tab")

        assert cmd == [
            "/usr/bin/wtype",
            "-d",
            "50",
            "-M",
            "shift",
            "-k",
            "Tab",
            "-m",
            "shift",
        ]

    @patch("dictation_app.injector.shutil.which")
    def test_resolve_wtype_key_action_enter(self, mock_which):
        """Enter command resolves correctly."""
        mock_which.return_value = "/usr/bin/wtype"

        config = InjectorConfig(backend="wtype", typing_delay=0)
        injector = Injector(config)

        cmd = injector._resolve_wtype_key_action("enter")

        assert cmd == ["/usr/bin/wtype", "-k", "Return"]

    @patch("dictation_app.injector.shutil.which")
    def test_resolve_ydotool_key_action_shift_tab(self, mock_which):
        """Shift+Tab with ydotool uses keycodes."""
        mock_which.return_value = "/usr/bin/ydotool"

        config = InjectorConfig(backend="ydotool", typing_delay=25)
        injector = Injector(config)

        cmd = injector._resolve_ydotool_key_action("shift_tab")

        assert cmd == [
            "/usr/bin/ydotool",
            "key",
            "-d",
            "25",
            "42:1",
            "15:1",
            "15:0",
            "42:0",
        ]

    @patch("dictation_app.injector.shutil.which")
    def test_resolve_ydotool_key_action_enter(self, mock_which):
        """Enter action uses correct keycodes."""
        mock_which.return_value = "/usr/bin/ydotool"

        config = InjectorConfig(backend="ydotool", typing_delay=0)
        injector = Injector(config)

        cmd = injector._resolve_ydotool_key_action("enter")

        assert cmd == ["/usr/bin/ydotool", "key", "28:1", "28:0"]

    @patch("dictation_app.injector.shutil.which")
    def test_resolve_xdotool_key_action_shift_tab(self, mock_which):
        """Shift+Tab for xdotool uses clearmodifiers."""
        mock_which.return_value = "/usr/bin/xdotool"

        config = InjectorConfig(backend="xdotool", typing_delay=10)
        injector = Injector(config)

        cmd = injector._resolve_xdotool_key_action("shift_tab")

        assert cmd[:2] == ["/usr/bin/xdotool", "key"]
        assert "--clearmodifiers" in cmd
        assert "Shift+Tab" in cmd
        assert "--delay" in cmd

    @patch("dictation_app.injector.shutil.which")
    def test_resolve_xdotool_key_action_enter(self, mock_which):
        """Enter action resolves for xdotool."""
        mock_which.return_value = "/usr/bin/xdotool"

        config = InjectorConfig(backend="xdotool", typing_delay=0)
        injector = Injector(config)

        cmd = injector._resolve_xdotool_key_action("enter")

        assert "Return" in cmd

    @patch("dictation_app.injector.shutil.which")
    @pytest.mark.asyncio
    async def test_press_key_action_wtype(self, mock_which):
        """press_key_action executes resolved command."""
        mock_which.return_value = "/usr/bin/wtype"

        config = InjectorConfig(backend="wtype", typing_delay=40)
        injector = Injector(config)

        with patch.object(injector, "_run_key_command", new_callable=AsyncMock) as mock_run:
            await injector.press_key_action("shift_tab")

        mock_run.assert_awaited_once()
        cmd, action = mock_run.await_args.args
        assert action == "shift_tab"
        assert cmd == [
            "/usr/bin/wtype",
            "-d",
            "40",
            "-M",
            "shift",
            "-k",
            "Tab",
            "-m",
            "shift",
        ]

    @patch("dictation_app.injector.shutil.which")
    @pytest.mark.asyncio
    async def test_press_key_action_ydotool(self, mock_which):
        """press_key_action works for ydotool backend."""
        mock_which.return_value = "/usr/bin/ydotool"

        config = InjectorConfig(backend="ydotool", typing_delay=30)
        injector = Injector(config)

        with patch.object(injector, "_run_key_command", new_callable=AsyncMock) as mock_run:
            await injector.press_key_action("enter")

        mock_run.assert_awaited_once()
        cmd, action = mock_run.await_args.args
        assert action == "enter"
        assert cmd == [
            "/usr/bin/ydotool",
            "key",
            "-d",
            "30",
            "28:1",
            "28:0",
        ]

    @patch("dictation_app.injector.shutil.which")
    @pytest.mark.asyncio
    async def test_press_key_action_xdotool(self, mock_which):
        """press_key_action works for xdotool backend."""
        mock_which.return_value = "/usr/bin/xdotool"

        config = InjectorConfig(backend="xdotool", typing_delay=15)
        injector = Injector(config)

        with patch.object(injector, "_run_key_command", new_callable=AsyncMock) as mock_run:
            await injector.press_key_action("enter")

        mock_run.assert_awaited_once()
        cmd, action = mock_run.await_args.args
        assert action == "enter"
        assert cmd[:2] == ["/usr/bin/xdotool", "key"]
        assert "--delay" in cmd
        assert "Return" in cmd

    @patch("dictation_app.injector.shutil.which")
    @pytest.mark.asyncio
    async def test_press_key_action_dry_run(self, mock_which):
        """Dry-run skips execution."""
        mock_which.return_value = "/usr/bin/wtype"

        config = InjectorConfig(backend="wtype", dry_run=True)
        injector = Injector(config)

        with patch.object(injector, "_run_key_command", new_callable=AsyncMock) as mock_run:
            await injector.press_key_action("enter")

        mock_run.assert_not_called()
class TestDryRunMode:
    """Tests for dry-run mode functionality."""

    @patch("dictation_app.injector.shutil.which")
    @pytest.mark.asyncio
    async def test_dry_run_wtype_logs_without_executing(self, mock_which):
        """Test dry-run mode logs command without executing subprocess."""
        mock_which.return_value = "/usr/bin/wtype"

        config = InjectorConfig(backend="wtype", dry_run=True)
        injector = Injector(config)

        with patch("dictation_app.injector.logger") as mock_logger:
            await injector.inject_text("hello world")
            mock_logger.info.assert_called()
            call_args = str(mock_logger.info.call_args)
            assert "[DRY-RUN]" in call_args

    @patch("dictation_app.injector.shutil.which")
    @pytest.mark.asyncio
    async def test_dry_run_ydotool_logs_without_executing(self, mock_which):
        """Test dry-run mode with ydotool backend."""
        mock_which.return_value = "/usr/bin/ydotool"

        config = InjectorConfig(backend="ydotool", dry_run=True)
        injector = Injector(config)

        with patch("dictation_app.injector.logger") as mock_logger:
            await injector.inject_text("hello world")
            mock_logger.info.assert_called()
            call_args = str(mock_logger.info.call_args)
            assert "[DRY-RUN]" in call_args

    @patch("dictation_app.injector.shutil.which")
    @pytest.mark.asyncio
    async def test_dry_run_with_clipboard_fallback_notes_fallback(self, mock_which):
        """Test dry-run mode notes when clipboard fallback is enabled."""
        mock_which.return_value = "/usr/bin/wtype"

        def which_side_effect(binary):
            return f"/usr/bin/{binary}"

        mock_which.side_effect = which_side_effect

        config = InjectorConfig(backend="wtype", clipboard_mode=True, dry_run=True)
        injector = Injector(config)

        with patch("dictation_app.injector.logger") as mock_logger:
            await injector.inject_text("hello")
            log_output = str(mock_logger.info.call_args)
            assert "clipboard fallback" in log_output


class TestWtypeInjection:
    """Tests for wtype backend injection."""

    @patch("dictation_app.injector.shutil.which")
    @patch("dictation_app.injector.subprocess.run")
    @pytest.mark.asyncio
    async def test_inject_wtype_success(self, mock_run, mock_which):
        """Test successful text injection via wtype."""
        mock_which.return_value = "/usr/bin/wtype"
        mock_run.return_value = MagicMock(returncode=0, stderr=None)

        config = InjectorConfig(backend="wtype", dry_run=False)
        injector = Injector(config)

        await injector.inject_text("hello world")

        mock_run.assert_called_once()
        call_args = mock_run.call_args
        assert "/usr/bin/wtype" in call_args[0][0]

    @patch("dictation_app.injector.shutil.which")
    @patch("dictation_app.injector.subprocess.run")
    @pytest.mark.asyncio
    async def test_inject_wtype_scales_timeout_for_long_text(self, mock_run, mock_which):
        """Ensure timeout scales when typing delay would exceed base timeout."""
        mock_which.return_value = "/usr/bin/wtype"
        mock_run.return_value = MagicMock(returncode=0, stderr=None)

        config = InjectorConfig(backend="wtype", typing_delay=25, timeout=5.0, dry_run=False)
        injector = Injector(config)

        captured_timeout = {}

        async def wait_for_stub(coro, timeout):
            captured_timeout["value"] = timeout
            return await coro

        with patch("dictation_app.injector.asyncio.wait_for", side_effect=wait_for_stub):
            await injector.inject_text("x" * 400)

        assert captured_timeout["value"] > config.timeout

    @patch("dictation_app.injector.shutil.which")
    @patch("dictation_app.injector.subprocess.run")
    @pytest.mark.asyncio
    async def test_inject_wtype_failure_non_zero_exit(self, mock_run, mock_which):
        """Test wtype injection failure with non-zero exit code."""
        mock_which.return_value = "/usr/bin/wtype"
        mock_run.return_value = MagicMock(
            returncode=1, stderr=b"error message"
        )

        config = InjectorConfig(backend="wtype", clipboard_mode=False, dry_run=False)
        injector = Injector(config)

        with pytest.raises(RuntimeError) as exc_info:
            await injector.inject_text("hello")

        assert "exit code 1" in str(exc_info.value)
        assert "error message" in str(exc_info.value)

    @patch("dictation_app.injector.shutil.which")
    @pytest.mark.asyncio
    async def test_inject_wtype_timeout(self, mock_which):
        """Test wtype injection timeout handling."""
        mock_which.return_value = "/usr/bin/wtype"

        config = InjectorConfig(backend="wtype", timeout=0.001, clipboard_mode=False, dry_run=False)
        injector = Injector(config)

        async def slow_runner(*args, **kwargs):
            await asyncio.sleep(10)
            return MagicMock(returncode=0)

        with patch.object(injector, "_resolve_wtype_command", return_value=["/usr/bin/wtype", "--", "hello"]):
            with patch("dictation_app.injector.asyncio.wait_for", side_effect=asyncio.TimeoutError()):
                with pytest.raises(TimeoutError) as exc_info:
                    await injector.inject_text("hello")

                assert "timed out" in str(exc_info.value).lower()


class TestYdotoolInjection:
    """Tests for ydotool backend injection."""

    @patch("dictation_app.injector.shutil.which")
    @patch("dictation_app.injector.subprocess.run")
    @pytest.mark.asyncio
    async def test_inject_ydotool_success(self, mock_run, mock_which):
        """Test successful text injection via ydotool."""
        mock_which.return_value = "/usr/bin/ydotool"
        mock_run.return_value = MagicMock(returncode=0, stderr=None)

        config = InjectorConfig(backend="ydotool", dry_run=False)
        injector = Injector(config)

        await injector.inject_text("hello world")

        mock_run.assert_called_once()
        call_args = mock_run.call_args
        assert "/usr/bin/ydotool" in call_args[0][0]

    @patch("dictation_app.injector.shutil.which")
    @patch("dictation_app.injector.subprocess.run")
    @pytest.mark.asyncio
    async def test_inject_ydotool_failure_non_zero_exit(self, mock_run, mock_which):
        """Test ydotool injection failure with non-zero exit code."""
        mock_which.return_value = "/usr/bin/ydotool"
        mock_run.return_value = MagicMock(
            returncode=127, stderr=b"command not executable"
        )

        config = InjectorConfig(backend="ydotool", clipboard_mode=False, dry_run=False)
        injector = Injector(config)

        with pytest.raises(RuntimeError) as exc_info:
            await injector.inject_text("hello")

        assert "exit code 127" in str(exc_info.value)


class TestXdotoolInjection:
    """Tests for xdotool backend injection."""

    @patch("dictation_app.injector.shutil.which")
    @patch("dictation_app.injector.subprocess.run")
    @pytest.mark.asyncio
    async def test_inject_xdotool_success(self, mock_run, mock_which):
        """Test successful text injection via xdotool."""
        mock_which.return_value = "/usr/bin/xdotool"
        mock_run.return_value = MagicMock(returncode=0, stderr=None)

        config = InjectorConfig(backend="xdotool", dry_run=False)
        injector = Injector(config)

        await injector.inject_text("hello world")

        mock_run.assert_called_once()
        call_args = mock_run.call_args
        assert "/usr/bin/xdotool" in call_args[0][0]

    @patch("dictation_app.injector.shutil.which")
    @patch("dictation_app.injector.subprocess.run")
    @pytest.mark.asyncio
    async def test_inject_xdotool_failure_non_zero_exit(self, mock_run, mock_which):
        """Test xdotool injection failure with non-zero exit code."""
        mock_which.return_value = "/usr/bin/xdotool"
        mock_run.return_value = MagicMock(
            returncode=2, stderr=b"xdotool error"
        )

        config = InjectorConfig(backend="xdotool", clipboard_mode=False, dry_run=False)
        injector = Injector(config)

        with pytest.raises(RuntimeError) as exc_info:
            await injector.inject_text("hello")

        assert "exit code 2" in str(exc_info.value)


class TestClipboardFallback:
    """Tests for clipboard fallback functionality."""

    @patch("dictation_app.injector.shutil.which")
    @pytest.mark.asyncio
    async def test_clipboard_fallback_activated_on_failure(self, mock_which):
        """Test clipboard fallback is activated when primary injection fails."""

        def which_side_effect(binary):
            return f"/usr/bin/{binary}"

        mock_which.side_effect = which_side_effect

        config = InjectorConfig(backend="wtype", clipboard_mode=True, dry_run=False)
        injector = Injector(config)

        call_count = [0]

        async def execute_injection_mock(text):
            call_count[0] += 1
            if call_count[0] == 1:
                raise RuntimeError("Primary injection failed")

        with patch.object(injector, "_execute_injection", side_effect=execute_injection_mock):
            with patch.object(injector, "_clipboard_fallback", new_callable=AsyncMock) as mock_fallback:
                await injector.inject_text("hello world")
                mock_fallback.assert_called_once_with("hello world")

    @patch("dictation_app.injector.shutil.which")
    @patch("dictation_app.injector.subprocess.run")
    @pytest.mark.asyncio
    async def test_clipboard_fallback_skipped_when_disabled(self, mock_run, mock_which):
        """Test clipboard fallback is skipped when clipboard_mode=False."""
        mock_which.return_value = "/usr/bin/wtype"
        mock_run.return_value = MagicMock(returncode=1, stderr=b"failed")

        config = InjectorConfig(backend="wtype", clipboard_mode=False, dry_run=False)
        injector = Injector(config)

        with pytest.raises(RuntimeError):
            await injector.inject_text("hello world")

        assert mock_run.call_count == 1

    @patch("dictation_app.injector.shutil.which")
    @patch("dictation_app.injector.subprocess.run")
    @pytest.mark.asyncio
    async def test_clipboard_fallback_wl_copy_failure(self, mock_run, mock_which):
        """Test error handling when wl-copy fails."""

        def which_side_effect(binary):
            return f"/usr/bin/{binary}"

        mock_which.side_effect = which_side_effect

        def run_side_effect(*args, **kwargs):
            cmd = args[0]
            if any("wl-copy" in token for token in cmd):
                return MagicMock(returncode=1, stderr=b"clipboard error")
            return MagicMock(returncode=1, stderr=b"primary failed")

        mock_run.side_effect = run_side_effect

        config = InjectorConfig(backend="wtype", clipboard_mode=True, dry_run=False)
        injector = Injector(config)

        with pytest.raises(RuntimeError) as exc_info:
            await injector.inject_text("hello")

        assert "wl-copy" in str(exc_info.value) or "clipboard error" in str(exc_info.value)

    @patch("dictation_app.injector.shutil.which")
    @patch("dictation_app.injector.subprocess.run")
    @pytest.mark.asyncio
    async def test_clipboard_fallback_xclip_failure(self, mock_run, mock_which):
        """Test clipboard helper failure on xdotool backend."""

        def which_side_effect(binary):
            return f"/usr/bin/{binary}"

        mock_which.side_effect = which_side_effect

        def run_side_effect(*args, **kwargs):
            cmd = args[0]
            if any("xclip" in token for token in cmd):
                return MagicMock(returncode=1, stderr=b"xclip error")
            return MagicMock(returncode=1, stderr=b"primary failed")

        mock_run.side_effect = run_side_effect

        config = InjectorConfig(backend="xdotool", clipboard_mode=True, dry_run=False)
        injector = Injector(config)

        with pytest.raises(RuntimeError) as exc_info:
            await injector.inject_text("hello")

        assert "xclip" in str(exc_info.value) or "xclip error" in str(exc_info.value)


class TestErrorHandling:
    """Tests for error handling and messaging."""

    @patch("dictation_app.injector.shutil.which")
    def test_command_not_found_error_message(self, mock_which):
        """Test CommandNotFoundError provides helpful message."""
        mock_which.return_value = None

        config = InjectorConfig(backend="wtype")

        with pytest.raises(CommandNotFoundError) as exc_info:
            Injector(config)

        assert "wtype" in str(exc_info.value)
        assert "not found" in str(exc_info.value).lower()

    @patch("dictation_app.injector.shutil.which")
    @patch("dictation_app.injector.subprocess.run")
    @pytest.mark.asyncio
    async def test_runtime_error_includes_stderr(self, mock_run, mock_which):
        """Test RuntimeError includes subprocess stderr in message."""
        mock_which.return_value = "/usr/bin/wtype"
        mock_run.return_value = MagicMock(
            returncode=1, stderr=b"specific error message"
        )

        config = InjectorConfig(backend="wtype", clipboard_mode=False)
        injector = Injector(config)

        with pytest.raises(RuntimeError) as exc_info:
            await injector.inject_text("hello")

        assert "specific error message" in str(exc_info.value)

    @patch("dictation_app.injector.shutil.which")
    @pytest.mark.asyncio
    async def test_timeout_error_message(self, mock_which):
        """Test TimeoutError message is descriptive."""
        mock_which.return_value = "/usr/bin/wtype"

        config = InjectorConfig(backend="wtype", timeout=5.0, clipboard_mode=False)
        injector = Injector(config)

        with patch.object(injector, "_resolve_wtype_command", return_value=["/usr/bin/wtype", "--", "hello"]):
            with patch("dictation_app.injector.asyncio.wait_for", side_effect=asyncio.TimeoutError()):
                with pytest.raises(TimeoutError) as exc_info:
                    await injector.inject_text("hello")

                assert "timed out" in str(exc_info.value).lower()


class TestSpecialCharacters:
    """Tests for handling special characters and edge cases."""

    @patch("dictation_app.injector.shutil.which")
    @pytest.mark.asyncio
    async def test_inject_with_special_characters(self, mock_which):
        """Test injection with special characters in text."""
        mock_which.return_value = "/usr/bin/wtype"

        config = InjectorConfig(backend="wtype", dry_run=True)
        injector = Injector(config)

        special_text = "hello @world! #test $100"
        with patch("dictation_app.injector.logger"):
            await injector.inject_text(special_text)

    @patch("dictation_app.injector.shutil.which")
    @pytest.mark.asyncio
    async def test_inject_with_quotes(self, mock_which):
        """Test injection with quotes in text."""
        mock_which.return_value = "/usr/bin/wtype"

        config = InjectorConfig(backend="wtype", dry_run=True)
        injector = Injector(config)

        quoted_text = 'hello "world" and \'test\''
        with patch("dictation_app.injector.logger"):
            await injector.inject_text(quoted_text)

    @patch("dictation_app.injector.shutil.which")
    @pytest.mark.asyncio
    async def test_inject_with_newlines_in_text(self, mock_which):
        """Test injection with multiple newlines in text."""
        mock_which.return_value = "/usr/bin/wtype"

        config = InjectorConfig(backend="wtype", dry_run=True)
        injector = Injector(config)

        multiline_text = "line1\nline2\nline3"
        with patch("dictation_app.injector.logger"):
            await injector.inject_text(multiline_text)

    @patch("dictation_app.injector.shutil.which")
    @pytest.mark.asyncio
    async def test_inject_empty_string(self, mock_which):
        """Test injection with empty string."""
        mock_which.return_value = "/usr/bin/wtype"

        config = InjectorConfig(backend="wtype", dry_run=True)
        injector = Injector(config)

        with patch("dictation_app.injector.logger"):
            await injector.inject_text("")
