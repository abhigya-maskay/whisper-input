"""Text injection into focused window."""

import logging
import subprocess

logger = logging.getLogger(__name__)


class Injector:
    """Sends text as synthetic keystrokes or clipboard paste to focused window.

    Supports wtype (default) or ydotool backends with optional clipboard fallback.
    """

    def __init__(self, backend: str = "wtype", clipboard_mode: bool = False):
        """Initialize injector.

        Args:
            backend: Injection backend (wtype or ydotool)
            clipboard_mode: Use clipboard paste fallback if direct typing fails
        """
        self.backend = backend
        self.clipboard_mode = clipboard_mode
        logger.info(
            "Injector initialized: backend=%s, clipboard_mode=%s",
            backend,
            clipboard_mode,
        )

    async def inject(self, text: str) -> None:
        """Inject text into focused window.

        Args:
            text: Text to inject

        TODO: Execute wtype/ydotool subprocess, handle errors, use clipboard fallback if needed
        """
        logger.info("Injecting text (length=%d)", len(text))
        raise NotImplementedError("inject() not yet implemented")

    @staticmethod
    def _run_command(command: list[str]) -> int:
        """Run shell command and return exit code.

        Args:
            command: Command as list of strings

        Returns:
            Exit code

        TODO: Execute subprocess.run with proper error handling
        """
        raise NotImplementedError("_run_command() not yet implemented")
