"""Central async state machine orchestrating all components."""

import asyncio
import logging
from enum import Enum

from dictation_app.button_listener import ButtonListener
from dictation_app.injector import Injector
from dictation_app.recorder import AudioRecorder
from dictation_app.transcriber import Transcriber

logger = logging.getLogger(__name__)


class State(Enum):
    """Orchestrator state."""

    IDLE = "idle"
    RECORDING = "recording"
    TRANSCRIBING = "transcribing"
    INJECTING = "injecting"
    ERROR = "error"


class Orchestrator:
    """Coordinates button listener, recorder, transcriber, and injector.

    Central state machine handling lifecycle events, timeouts, and error propagation.
    """

    def __init__(
        self,
        button_listener: ButtonListener,
        recorder: AudioRecorder,
        transcriber: Transcriber,
        injector: Injector,
    ):
        """Initialize orchestrator with components.

        Args:
            button_listener: ButtonListener instance
            recorder: AudioRecorder instance
            transcriber: Transcriber instance
            injector: Injector instance
        """
        self.button_listener = button_listener
        self.recorder = recorder
        self.transcriber = transcriber
        self.injector = injector
        self.state = State.IDLE
        logger.info("Orchestrator initialized in IDLE state")

    async def run(self) -> None:
        """Main event loop coordinating all components.

        Handles:
        - Button press -> start recording
        - Button release -> stop recording, start transcription
        - Transcription -> inject text
        - Errors -> log and recover to IDLE

        TODO: Implement state machine with button listener, recorder, transcriber, injector
        """
        logger.info("Orchestrator event loop starting")
        try:
            # TODO: Main async loop with error handling and graceful shutdown
            raise NotImplementedError("run() not yet implemented")
        except Exception as e:
            logger.error("Orchestrator error: %s", e, exc_info=True)
            self.state = State.ERROR
            raise
