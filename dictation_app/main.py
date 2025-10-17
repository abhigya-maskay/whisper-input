"""Typer CLI entrypoint for dictation-app."""

import json
import logging
from pathlib import Path

import typer

from dictation_app.config import (
    ConfigError,
    discover_audio_devices,
    discover_input_devices,
    load_config,
)

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
    except ConfigError as e:
        logger.error("Configuration error: %s", e)
        raise typer.Exit(1)
    except Exception as e:
        logger.error("Unexpected error: %s", e)
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
