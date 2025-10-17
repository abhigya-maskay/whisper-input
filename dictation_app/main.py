"""Typer CLI entrypoint for dictation-app."""

import logging
from pathlib import Path

import typer

app = typer.Typer(help="Hyprland dictation helper via Faster Whisper")

logger = logging.getLogger(__name__)


@app.command()
def run(
    config: Path = typer.Option(
        "dictation.toml", "--config", help="Path to configuration file"
    ),
    verbose: bool = typer.Option(
        False, "--verbose", "-v", help="Enable verbose logging"
    ),
) -> None:
    """Run the dictation helper daemon."""
    logging.basicConfig(
        level=logging.DEBUG if verbose else logging.INFO,
        format="%(asctime)s - %(name)s - %(levelname)s - %(message)s",
    )
    logger.info("Starting dictation helper with config: %s", config)
    # TODO: Initialize config, button listener, orchestrator and start event loop


@app.command()
def list_inputs(verbose: bool = typer.Option(False, "--verbose", "-v")) -> None:
    """List available input devices."""
    logging.basicConfig(
        level=logging.DEBUG if verbose else logging.INFO,
        format="%(asctime)s - %(name)s - %(levelname)s - %(message)s",
    )
    logger.info("Listing input devices...")
    # TODO: Enumerate /dev/input/by-id devices and display to user


@app.command()
def list_audio(verbose: bool = typer.Option(False, "--verbose", "-v")) -> None:
    """List available audio devices."""
    logging.basicConfig(
        level=logging.DEBUG if verbose else logging.INFO,
        format="%(asctime)s - %(name)s - %(levelname)s - %(message)s",
    )
    logger.info("Listing audio devices...")
    # TODO: Query sounddevice and display available capture devices


@app.command()
def dry_run(
    config: Path = typer.Option(
        "dictation.toml", "--config", help="Path to configuration file"
    ),
    verbose: bool = typer.Option(False, "--verbose", "-v"),
) -> None:
    """Run a dry-run without injection (for testing)."""
    logging.basicConfig(
        level=logging.DEBUG if verbose else logging.INFO,
        format="%(asctime)s - %(name)s - %(levelname)s - %(message)s",
    )
    logger.info("Running in dry-run mode with config: %s", config)
    # TODO: Run orchestrator but skip injector step for testing


if __name__ == "__main__":
    app()
