"""Tests for orchestrator state machine."""

import pytest


class TestOrchestrator:
    """Tests for orchestrator coordination."""

    @pytest.mark.skip(reason="Implementation pending")
    @pytest.mark.asyncio
    async def test_orchestrator_state_machine(self):
        """Test orchestrator state transitions.

        TODO: Mock all components, verify IDLE -> RECORDING -> TRANSCRIBING -> INJECTING -> IDLE
        """
        pass

    @pytest.mark.skip(reason="Implementation pending")
    @pytest.mark.asyncio
    async def test_button_press_starts_recording(self):
        """Test button press triggers recording.

        TODO: Mock button listener, verify recorder starts
        """
        pass

    @pytest.mark.skip(reason="Implementation pending")
    @pytest.mark.asyncio
    async def test_button_release_stops_recording(self):
        """Test button release stops recording and starts transcription.

        TODO: Mock recorder, verify file path passed to transcriber
        """
        pass

    @pytest.mark.skip(reason="Implementation pending")
    @pytest.mark.asyncio
    async def test_error_handling(self):
        """Test error recovery without exiting.

        TODO: Mock component failures, verify recovery to IDLE
        """
        pass

    @pytest.mark.skip(reason="Implementation pending")
    @pytest.mark.asyncio
    async def test_transcription_timeout(self):
        """Test transcription timeout handling.

        TODO: Verify timeout cancellation and error recovery
        """
        pass
