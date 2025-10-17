"""Typer CLI entrypoint for dictation-app."""

import asyncio
import json
import logging
from pathlib import Path

import typer

from dictation_app.button_listener import ButtonListener
from dictation_app.config import (
    ConfigError,
    discover_audio_devices,
    discover_input_devices,
    load_config,
)
from dictation_app.injector import Injector
from dictation_app.orchestrator import Orchestrator
from dictation_app.recorder import AudioRecorder
from dictation_app.transcriber import Transcriber

app = typer.Typer(help="Hyprland dictation helper via Faster Whisper")

logger = logging.getLogger(__name__)


def _setup_logging(verbose: bool = False) -> None:
    """Setup logging configuration."""
    logging.basicConfig(
        level=logging.DEBUG if verbose else logging.INFO,
        format="%(asctime)s - %(name)s - %(levelname)s - %(message)s",
    )


@app.command()
def run(
    config: Path | None = typer.Option(
        None, "--config", help="Path to configuration file"
    ),
    verbose: bool = typer.Option(
        False, "--verbose", "-v", help="Enable verbose logging"
    ),
) -> None:
    """Run the dictation helper daemon."""
    _setup_logging(verbose)
    try:
        cfg = load_config(config)
        logger.info("Loaded config from: %s", config or "default locations")
        logger.debug("Config: %s", cfg)
        cfg.validate()
        logger.info("Configuration validated successfully")

        # Initialize components
        button_listener = ButtonListener.from_config(cfg.input)
        recorder = AudioRecorder(
            sample_rate=cfg.audio.sample_rate,
            channels=cfg.audio.channels,
            chunk_size=cfg.audio.chunk_size,
            trim_silence=True,
        )
        transcriber = Transcriber(
            model_name=cfg.model.name,
            device=cfg.model.device,
            compute_type=cfg.model.compute_type,
            model_directory=cfg.model.model_directory,
            beam_size=cfg.model.beam_size,
        )
        injector = Injector(cfg.injector)
        orchestrator = Orchestrator(
            button_listener=button_listener,
            recorder=recorder,
            transcriber=transcriber,
            injector=injector,
            config=cfg.orchestrator,
        )

        # Run the orchestrator
        logger.info("Starting dictation daemon")
        asyncio.run(orchestrator.run())

    except ConfigError as e:
        logger.error("Configuration error: %s", e)
        raise typer.Exit(1)
    except KeyboardInterrupt:
        logger.info("Daemon interrupted by user")
        raise typer.Exit(0)
    except Exception as e:
        logger.error("Unexpected error: %s", e, exc_info=True)
        raise typer.Exit(1)


@app.command()
def list_inputs(
    verbose: bool = typer.Option(False, "--verbose", "-v"),
    json_output: bool = typer.Option(
        False, "--json", help="Output as JSON instead of table"
    ),
) -> None:
    """List available input devices."""
    _setup_logging(verbose)
    try:
        devices = discover_input_devices()
        if not devices:
            logger.warning("No input devices found")
            return

        if json_output:
            typer.echo(json.dumps(devices, indent=2))
        else:
            typer.echo("Available input devices:")
            for dev in devices:
                typer.echo(f"  {dev['path']}")
                typer.echo(f"    Name: {dev['name']}")
                typer.echo(f"    Capabilities: {dev['capabilities']}")
    except Exception as e:
        logger.error("Error listing input devices: %s", e)
        raise typer.Exit(1)


@app.command()
def list_audio(
    verbose: bool = typer.Option(False, "--verbose", "-v"),
    json_output: bool = typer.Option(
        False, "--json", help="Output as JSON instead of table"
    ),
) -> None:
    """List available audio devices."""
    _setup_logging(verbose)
    try:
        devices = discover_audio_devices()
        if not devices:
            logger.warning("No audio devices found")
            return

        if json_output:
            typer.echo(json.dumps(devices, indent=2))
        else:
            typer.echo("Available audio devices:")
            for dev in devices:
                typer.echo(
                    f"  [{dev['index']}] {dev['name']} "
                    f"({dev['channels']}ch, {dev['sample_rate']}Hz)"
                )
    except Exception as e:
        logger.error("Error listing audio devices: %s", e)
        raise typer.Exit(1)


@app.command()
def dry_run(
    config: Path | None = typer.Option(
        None, "--config", help="Path to configuration file"
    ),
    verbose: bool = typer.Option(False, "--verbose", "-v"),
) -> None:
    """Run a dry-run without injection (for testing)."""
    _setup_logging(verbose)
    try:
        cfg = load_config(config)
        logger.info("Loaded config from: %s", config or "default locations")
        cfg.validate()
        logger.info("Configuration validated successfully (dry-run mode)")
        logger.info("Would start orchestrator with dry-run injector")
    except ConfigError as e:
        logger.error("Configuration error: %s", e)
        raise typer.Exit(1)
    except Exception as e:
        logger.error("Unexpected error: %s", e)
        raise typer.Exit(1)


if __name__ == "__main__":
    app()
