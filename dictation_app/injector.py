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
    """Required binary (wtype/ydotool/wl-copy) is not available or not executable."""

    pass


class TimeoutError(InjectionError):
    """Subprocess execution timed out."""

    pass


class Injector:
    """Sends text as synthetic keystrokes or clipboard paste to focused window.

    Supports wtype (default) or ydotool backends with optional clipboard fallback.
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
        if self.clipboard_mode:
            self._validate_binary("wl-copy")

    def _validate_binary(self, binary_name: str) -> Path:
        """Validate that binary exists and is executable.

        Args:
            binary_name: Name of binary (e.g., "wtype", "ydotool", "wl-copy")

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
            cmd.extend(["--delay", str(self.typing_delay)])
        cmd.extend(["--", text])
        if self.use_newline:
            cmd[-1] = text + "\n"
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

        loop = asyncio.get_event_loop()
        try:
            result = await asyncio.wait_for(
                loop.run_in_executor(
                    None,
                    subprocess.run,
                    cmd,
                    None,  # stdin
                    subprocess.PIPE,  # stdout
                    subprocess.PIPE,  # stderr
                ),
                timeout=self.timeout,
            )
        except asyncio.TimeoutError as e:
            raise TimeoutError(
                f"wtype injection timed out after {self.timeout}s"
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

        loop = asyncio.get_event_loop()
        try:
            result = await asyncio.wait_for(
                loop.run_in_executor(
                    None,
                    subprocess.run,
                    cmd,
                    None,  # stdin
                    subprocess.PIPE,  # stdout
                    subprocess.PIPE,  # stderr
                ),
                timeout=self.timeout,
            )
        except asyncio.TimeoutError as e:
            raise TimeoutError(
                f"ydotool injection timed out after {self.timeout}s"
            ) from e

        if result.returncode != 0:
            stderr = result.stderr.decode("utf-8", errors="replace") if result.stderr else ""
            raise RuntimeError(
                f"ydotool failed with exit code {result.returncode}. "
                f"Command: {' '.join(cmd)} | stderr: {stderr}"
            )

        logger.debug("ydotool injection succeeded")

    async def _clipboard_fallback(self, text: str) -> None:
        """Fallback: pipe text to wl-copy and retry typing with --paste.

        First pipes text into wl-copy to populate clipboard, then retries
        the primary injection command exactly once using clipboard paste.

        Args:
            text: Text to inject

        Raises:
            TimeoutError: If subprocess exceeds timeout
            RuntimeError: If subprocess fails
        """
        logger.debug("Attempting clipboard fallback: piping to wl-copy")

        wl_copy_path = str(self._validate_binary("wl-copy"))
        loop = asyncio.get_event_loop()

        try:
            wl_result = await asyncio.wait_for(
                loop.run_in_executor(
                    None,
                    subprocess.run,
                    [wl_copy_path],
                    text.encode("utf-8"),
                    subprocess.PIPE,
                    subprocess.PIPE,
                ),
                timeout=self.timeout,
            )
        except asyncio.TimeoutError as e:
            raise TimeoutError(
                f"wl-copy timed out after {self.timeout}s"
            ) from e

        if wl_result.returncode != 0:
            stderr = (
                wl_result.stderr.decode("utf-8", errors="replace")
                if wl_result.stderr
                else ""
            )
            raise RuntimeError(
                f"wl-copy failed with exit code {wl_result.returncode}. "
                f"stderr: {stderr}"
            )

        logger.debug("wl-copy succeeded, retrying injection")

        if self.backend == "wtype":
            cmd = [str(self._validate_binary("wtype")), "--paste"]
            logger.debug("Executing wtype with --paste: %s", " ".join(cmd))
        elif self.backend == "ydotool":
            cmd = [str(self._validate_binary("ydotool")), "key", "ctrl+v"]
            logger.debug("Executing ydotool ctrl+v for paste: %s", " ".join(cmd))
        else:
            raise ValueError(f"Unknown backend: {self.backend}")

        try:
            result = await asyncio.wait_for(
                loop.run_in_executor(
                    None,
                    subprocess.run,
                    cmd,
                    None,  # stdin
                    subprocess.PIPE,  # stdout
                    subprocess.PIPE,  # stderr
                ),
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
