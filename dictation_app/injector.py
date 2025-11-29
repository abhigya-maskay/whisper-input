"""Text injection into focused window via synthetic keyboard events."""

import asyncio
import logging
import shutil
import subprocess
from pathlib import Path

from dictation_app.config import InjectorConfig

logger = logging.getLogger(__name__)


class InjectionError(Exception):
    """Base exception for injection failures."""

    pass


class CommandNotFoundError(InjectionError):
    """Required binary (wtype/ydotool/xdotool/clipboard helper) unavailable or not executable."""

    pass


class TimeoutError(InjectionError):
    """Subprocess execution timed out."""

    pass


class Injector:
    """Sends text as synthetic keystrokes or clipboard paste to focused window.

    Supports wtype (default), ydotool, or xdotool backends with optional clipboard fallback.
    Runs subprocess operations in thread pool to avoid blocking event loop.
    """

    def __init__(self, config: InjectorConfig):
        """Initialize injector and validate binary availability.

        Args:
            config: InjectorConfig instance with backend, clipboard settings, etc.

        Raises:
            CommandNotFoundError: If configured backend binary is unavailable
        """
        self.config = config
        self.backend = config.backend
        self.clipboard_mode = config.clipboard_mode
        self.typing_delay = config.typing_delay
        self.use_newline = config.use_newline
        self.timeout = config.timeout
        self.dry_run = config.dry_run
        self._clipboard_binary = self._determine_clipboard_binary()

        self._binary_cache = {}

        logger.info(
            "Injector initialized: backend=%s, clipboard_mode=%s, "
            "typing_delay=%dms, timeout=%.1fs, dry_run=%s",
            self.backend,
            self.clipboard_mode,
            self.typing_delay,
            self.timeout,
            self.dry_run,
        )

        self._validate_binary(self.backend)
        if self.clipboard_mode and self._clipboard_binary:
            self._validate_binary(self._clipboard_binary)

    def _validate_binary(self, binary_name: str) -> Path:
        """Validate that binary exists and is executable.

        Args:
            binary_name: Binary name (e.g., "wtype", "ydotool", "xdotool", "wl-copy", "xclip")

        Returns:
            Resolved Path to the binary

        Raises:
            CommandNotFoundError: If binary not found or not executable
        """
        if binary_name in self._binary_cache:
            return self._binary_cache[binary_name]

        binary_path = shutil.which(binary_name)
        if not binary_path:
            raise CommandNotFoundError(
                f"Binary '{binary_name}' not found in PATH. "
                f"Install it to enable text injection."
            )

        resolved = Path(binary_path)
        self._binary_cache[binary_name] = resolved
        logger.debug("Validated binary: %s -> %s", binary_name, resolved)
        return resolved

    def _determine_clipboard_binary(self) -> str | None:
        """Return clipboard binary name appropriate for backend."""
        if self.backend in ("wtype", "ydotool"):
            return "wl-copy"
        if self.backend == "xdotool":
            return "xclip"
        return None

    def _resolve_command(self, text: str) -> list[str]:
        """Resolve command arguments for the configured backend.

        Args:
            text: Text to inject

        Returns:
            Command as list of strings ready for subprocess execution
        """
        if self.backend == "wtype":
            return self._resolve_wtype_command(text)
        elif self.backend == "ydotool":
            return self._resolve_ydotool_command(text)
        elif self.backend == "xdotool":
            return self._resolve_xdotool_command(text)
        else:
            raise ValueError(f"Unknown backend: {self.backend}")

    def _resolve_wtype_command(self, text: str) -> list[str]:
        """Build wtype command with delay and optional newline.

        Args:
            text: Text to type

        Returns:
            Command list for subprocess
        """
        cmd = [str(self._validate_binary("wtype"))]
        if self.typing_delay > 0:
            cmd.extend(["-d", str(self.typing_delay)])
        if self.use_newline:
            cmd.append(text + "\n")
        else:
            cmd.append(text)
        return cmd

    def _resolve_ydotool_command(self, text: str) -> list[str]:
        """Build ydotool command with delay and optional newline.

        Args:
            text: Text to type

        Returns:
            Command list for subprocess
        """
        cmd = [str(self._validate_binary("ydotool")), "type"]
        if self.typing_delay > 0:
            cmd.extend(["--delay", str(self.typing_delay)])

        if self.use_newline:
            cmd.append(text + "\n")
        else:
            cmd.append(text)
        return cmd

    def _resolve_xdotool_command(self, text: str) -> list[str]:
        """Build xdotool command with delay, modifier clearing, and optional newline."""
        cmd = [str(self._validate_binary("xdotool")), "type"]
        if self.typing_delay > 0:
            cmd.extend(["--delay", str(self.typing_delay)])

        cmd.append("--clearmodifiers")
        cmd.append(text + "\n" if self.use_newline else text)
        return cmd

    def _estimate_typing_duration(self, text_length: int) -> float:
        """Estimate typing duration in seconds for the given text length."""
        if text_length <= 0:
            return 0.0
        delay_seconds = max(self.typing_delay, 0) / 1000.0
        return delay_seconds * text_length

    def _effective_timeout(self, text_length: int) -> tuple[float, float]:
        """Compute effective timeout and estimated typing duration."""
        estimated_duration = self._estimate_typing_duration(text_length)
        grace_period = 2.0
        effective_timeout = estimated_duration + grace_period
        if self.timeout and self.timeout > 0:
            effective_timeout = max(self.timeout, effective_timeout)
        return effective_timeout, estimated_duration

    async def inject_text(self, text: str) -> None:
        """Inject text into focused window via configured backend.

        Orchestrates primary injection path with fallback to clipboard if enabled.
        All subprocess operations run in thread pool to avoid blocking event loop.

        Args:
            text: Text to inject into focused window

        Raises:
            CommandNotFoundError: If binary is unavailable
            TimeoutError: If subprocess exceeds configured timeout
            RuntimeError: If subprocess exits with non-zero code
        """
        logger.debug("Injecting text (length=%d, backend=%s)", len(text), self.backend)

        if self.dry_run:
            cmd = self._resolve_command(text)
            fallback_info = (
                " with clipboard fallback on failure"
                if self.clipboard_mode
                else " (no fallback)"
            )
            logger.info(
                "[DRY-RUN] Would execute: %s%s",
                " ".join(cmd),
                fallback_info,
            )
            return

        try:
            await self._execute_injection(text)
        except (TimeoutError, RuntimeError) as e:
            if self.clipboard_mode:
                logger.warning(
                    "Primary injection failed (%s), attempting clipboard fallback",
                    type(e).__name__,
                )
                await self._clipboard_fallback(text)
            else:
                raise

    async def press_key_action(self, action: str) -> None:
        """Send a predefined key action (e.g., Shift+Tab, Enter)."""
        logger.debug("Dispatching key action '%s' via backend '%s'", action, self.backend)

        if self.dry_run:
            logger.info("[DRY-RUN] Would send key action '%s' via %s", action, self.backend)
            return

        if self.backend == "wtype":
            cmd = self._resolve_wtype_key_action(action)
        elif self.backend == "ydotool":
            cmd = self._resolve_ydotool_key_action(action)
        elif self.backend == "xdotool":
            cmd = self._resolve_xdotool_key_action(action)
        else:
            raise ValueError(f"Unknown backend: {self.backend}")

        await self._run_key_command(cmd, action)

    async def _execute_injection(self, text: str) -> None:
        """Execute primary injection via configured backend.

        Args:
            text: Text to inject

        Raises:
            TimeoutError: If subprocess exceeds timeout
            RuntimeError: If subprocess fails
        """
        if self.backend == "wtype":
            await self._inject_wtype(text)
        elif self.backend == "ydotool":
            await self._inject_ydotool(text)
        elif self.backend == "xdotool":
            await self._inject_xdotool(text)
        else:
            raise ValueError(f"Unknown backend: {self.backend}")

    async def _inject_wtype(self, text: str) -> None:
        """Execute text injection via wtype backend.

        Args:
            text: Text to inject

        Raises:
            TimeoutError: If subprocess exceeds timeout
            RuntimeError: If subprocess fails with non-zero exit code
        """
        cmd = self._resolve_wtype_command(text)
        logger.debug("Executing wtype: %s", " ".join(cmd))

        text_length = len(text)
        effective_timeout, estimated_duration = self._effective_timeout(text_length)
        if effective_timeout != self.timeout:
            logger.debug(
                "wtype timeout adjusted to %.2fs (base=%.2fs, estimated typing=%.2fs, chars=%d)",
                effective_timeout,
                self.timeout,
                estimated_duration,
                text_length,
            )

        loop = asyncio.get_event_loop()
        try:
            def _run_subprocess():
                return subprocess.run(
                    cmd,
                    stdin=None,
                    stdout=subprocess.PIPE,
                    stderr=subprocess.PIPE,
                )
            
            result = await asyncio.wait_for(
                loop.run_in_executor(None, _run_subprocess),
                timeout=effective_timeout,
            )
        except asyncio.TimeoutError as e:
            raise TimeoutError(
                f"wtype injection timed out after {effective_timeout}s"
            ) from e

        if result.returncode != 0:
            stderr = result.stderr.decode("utf-8", errors="replace") if result.stderr else ""
            raise RuntimeError(
                f"wtype failed with exit code {result.returncode}. "
                f"Command: {' '.join(cmd)} | stderr: {stderr}"
            )

        logger.debug("wtype injection succeeded")

    async def _inject_ydotool(self, text: str) -> None:
        """Execute text injection via ydotool backend.

        Args:
            text: Text to inject

        Raises:
            TimeoutError: If subprocess exceeds timeout
            RuntimeError: If subprocess fails with non-zero exit code
        """
        cmd = self._resolve_ydotool_command(text)
        logger.debug("Executing ydotool: %s", " ".join(cmd))

        text_length = len(text)
        effective_timeout, estimated_duration = self._effective_timeout(text_length)
        if effective_timeout != self.timeout:
            logger.debug(
                "ydotool timeout adjusted to %.2fs (base=%.2fs, estimated typing=%.2fs, chars=%d)",
                effective_timeout,
                self.timeout,
                estimated_duration,
                text_length,
            )

        loop = asyncio.get_event_loop()
        try:
            def _run_subprocess():
                return subprocess.run(
                    cmd,
                    stdin=None,
                    stdout=subprocess.PIPE,
                    stderr=subprocess.PIPE,
                )
            
            result = await asyncio.wait_for(
                loop.run_in_executor(None, _run_subprocess),
                timeout=effective_timeout,
            )
        except asyncio.TimeoutError as e:
            raise TimeoutError(
                f"ydotool injection timed out after {effective_timeout}s"
            ) from e

        if result.returncode != 0:
            stderr = result.stderr.decode("utf-8", errors="replace") if result.stderr else ""
            raise RuntimeError(
                f"ydotool failed with exit code {result.returncode}. "
                f"Command: {' '.join(cmd)} | stderr: {stderr}"
            )

        logger.debug("ydotool injection succeeded")

    async def _inject_xdotool(self, text: str) -> None:
        """Execute text injection via xdotool backend.

        Args:
            text: Text to inject

        Raises:
            TimeoutError: If subprocess exceeds timeout
            RuntimeError: If subprocess fails with non-zero exit code
        """
        cmd = self._resolve_xdotool_command(text)
        logger.debug("Executing xdotool: %s", " ".join(cmd))

        text_length = len(text)
        effective_timeout, estimated_duration = self._effective_timeout(text_length)
        if effective_timeout != self.timeout:
            logger.debug(
                "xdotool timeout adjusted to %.2fs (base=%.2fs, estimated typing=%.2fs, chars=%d)",
                effective_timeout,
                self.timeout,
                estimated_duration,
                text_length,
            )

        loop = asyncio.get_event_loop()
        try:
            def _run_subprocess():
                return subprocess.run(
                    cmd,
                    stdin=None,
                    stdout=subprocess.PIPE,
                    stderr=subprocess.PIPE,
                )
            
            result = await asyncio.wait_for(
                loop.run_in_executor(None, _run_subprocess),
                timeout=effective_timeout,
            )
        except asyncio.TimeoutError as e:
            raise TimeoutError(
                f"xdotool injection timed out after {effective_timeout}s"
            ) from e

        if result.returncode != 0:
            stderr = result.stderr.decode("utf-8", errors="replace") if result.stderr else ""
            raise RuntimeError(
                f"xdotool failed with exit code {result.returncode}. "
                f"Command: {' '.join(cmd)} | stderr: {stderr}"
            )

        logger.debug("xdotool injection succeeded")

    async def _clipboard_fallback(self, text: str) -> None:
        """Fallback: pipe text to clipboard helper then paste via backend command.

        Args:
            text: Text to inject

        Raises:
            TimeoutError: If subprocess exceeds timeout
            RuntimeError: If subprocess fails
        """
        clipboard_binary = self._clipboard_binary
        if not clipboard_binary:
            raise RuntimeError(f"Clipboard mode not supported for backend '{self.backend}'")

        logger.debug("Attempting clipboard fallback via %s", clipboard_binary)

        clipboard_cmd = [str(self._validate_binary(clipboard_binary))]
        if clipboard_binary == "xclip":
            clipboard_cmd.extend(["-selection", "clipboard"])

        loop = asyncio.get_event_loop()

        try:
            def _run_clipboard():
                return subprocess.run(
                    clipboard_cmd,
                    stdin=None,
                    stdout=subprocess.PIPE,
                    stderr=subprocess.PIPE,
                    input=text.encode("utf-8"),
                )
            
            clipboard_result = await asyncio.wait_for(
                loop.run_in_executor(None, _run_clipboard),
                timeout=self.timeout,
            )
        except asyncio.TimeoutError as e:
            raise TimeoutError(
                f"{clipboard_binary} timed out after {self.timeout}s"
            ) from e

        if clipboard_result.returncode != 0:
            stderr = (
                clipboard_result.stderr.decode("utf-8", errors="replace")
                if clipboard_result.stderr
                else ""
            )
            raise RuntimeError(
                f"{clipboard_binary} failed with exit code {clipboard_result.returncode}. "
                f"stderr: {stderr}"
            )

        logger.debug("%s succeeded, retrying injection", clipboard_binary)

        cmd = self._resolve_clipboard_paste_command()
        logger.debug("Executing clipboard paste command: %s", " ".join(cmd))

        try:
            def _run_subprocess():
                return subprocess.run(
                    cmd,
                    stdin=None,
                    stdout=subprocess.PIPE,
                    stderr=subprocess.PIPE,
                )
            
            result = await asyncio.wait_for(
                loop.run_in_executor(None, _run_subprocess),
                timeout=self.timeout,
            )
        except asyncio.TimeoutError as e:
            raise TimeoutError(
                f"Paste command timed out after {self.timeout}s"
            ) from e

        if result.returncode != 0:
            stderr = result.stderr.decode("utf-8", errors="replace") if result.stderr else ""
            raise RuntimeError(
                f"Paste command failed with exit code {result.returncode}. "
                f"Command: {' '.join(cmd)} | stderr: {stderr}"
            )

        logger.debug("Clipboard fallback injection succeeded")

    def _resolve_wtype_key_action(self, action: str) -> list[str]:
        """Resolve wtype command for a key action."""
        cmd = [str(self._validate_binary("wtype"))]
        if self.typing_delay > 0:
            cmd.extend(["-d", str(self.typing_delay)])

        if action == "shift_tab":
            cmd.extend(["-M", "shift", "-k", "Tab", "-m", "shift"])
        elif action == "enter":
            cmd.extend(["-k", "Return"])
        else:
            raise ValueError(f"Unknown key action: {action}")

        return cmd

    def _resolve_ydotool_key_action(self, action: str) -> list[str]:
        """Resolve ydotool command for a key action."""
        cmd = [str(self._validate_binary("ydotool")), "key"]
        if self.typing_delay > 0:
            cmd.extend(["-d", str(self.typing_delay)])

        if action == "shift_tab":
            cmd.extend(["42:1", "15:1", "15:0", "42:0"])
        elif action == "enter":
            cmd.extend(["28:1", "28:0"])
        else:
            raise ValueError(f"Unknown key action: {action}")

        return cmd

    def _resolve_xdotool_key_action(self, action: str) -> list[str]:
        """Resolve xdotool command for a key action."""
        cmd = [str(self._validate_binary("xdotool")), "key"]
        if self.typing_delay > 0:
            cmd.extend(["--delay", str(self.typing_delay)])

        cmd.append("--clearmodifiers")

        if action == "shift_tab":
            cmd.append("Shift+Tab")
        elif action == "enter":
            cmd.append("Return")
        else:
            raise ValueError(f"Unknown key action: {action}")

        return cmd

    def _resolve_clipboard_paste_command(self) -> list[str]:
        """Return the backend-specific clipboard paste command."""
        if self.backend == "wtype":
            return [str(self._validate_binary("wtype")), "--paste"]
        if self.backend == "ydotool":
            return [str(self._validate_binary("ydotool")), "key", "ctrl+v"]
        if self.backend == "xdotool":
            cmd = [str(self._validate_binary("xdotool")), "key"]
            if self.typing_delay > 0:
                cmd.extend(["--delay", str(self.typing_delay)])
            cmd.extend(["--clearmodifiers", "ctrl+v"])
            return cmd
        raise ValueError(f"Unknown backend: {self.backend}")

    async def _run_key_command(self, cmd: list[str], action: str) -> None:
        """Execute key action command with timeout handling."""
        logger.debug("Executing key action '%s': %s", action, " ".join(cmd))

        loop = asyncio.get_running_loop()

        def _run():
            return subprocess.run(
                cmd,
                stdin=None,
                stdout=subprocess.PIPE,
                stderr=subprocess.PIPE,
            )

        timeout = self.timeout if self.timeout and self.timeout > 0 else None

        try:
            result = await asyncio.wait_for(
                loop.run_in_executor(None, _run),
                timeout=timeout,
            )
        except asyncio.TimeoutError as e:
            if timeout is not None:
                message = f"{self.backend} key action '{action}' timed out after {timeout}s"
            else:
                message = f"{self.backend} key action '{action}' timed out"
            raise TimeoutError(message) from e

        if result.returncode != 0:
            stderr = (
                result.stderr.decode("utf-8", errors="replace")
                if result.stderr
                else ""
            )
            raise RuntimeError(
                f"{self.backend} key action '{action}' failed with exit code {result.returncode}. "
                f"stderr: {stderr}"
            )

        logger.debug("Key action '%s' succeeded", action)
