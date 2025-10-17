"""Typer CLI entrypoint for dictation-app."""

import asyncio
import json
import logging
from pathlib import Path

import typer

from dictation_app.button_listener import ButtonListener
from dictation_app.config import (
    Config,
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

VALID_MODELS = ("tiny", "base", "small", "medium", "large")
VALID_DEVICES = ("cpu", "cuda", "auto")


def _setup_logging(verbose: bool = False) -> None:
    """Setup logging configuration."""
    logging.basicConfig(
        level=logging.DEBUG if verbose else logging.INFO,
        format="%(asctime)s - %(name)s - %(levelname)s - %(message)s",
    )


def _merge_config_overrides(
    cfg: Config,
    *,
    audio_device: int | None = None,
    model: str | None = None,
    device: str | None = None,
    input_device: str | None = None,
    dry_run: bool = False,
) -> Config:
    """Apply CLI overrides to configuration.

    CLI options take precedence over config file values.

    Args:
        cfg: Base configuration from file
        audio_device: Override audio device by index
        model: Override model name
        device: Override compute device
        input_device: Override input device path
        dry_run: Enable dry-run mode in injector

    Returns:
        Updated Config instance

    Raises:
        ConfigError: If override values are invalid
    """
    if audio_device is not None:
        available = discover_audio_devices()
        valid_indices = {d["index"] for d in available}
        if audio_device not in valid_indices:
            available_str = ", ".join(str(d["index"]) for d in available)
            raise ConfigError(
                f"Invalid audio device index {audio_device}. "
                f"Available: {available_str or 'none'}"
            )
        logger.debug("Overriding audio device to index %d", audio_device)
        cfg.audio.device = audio_device

    if model is not None:
        if model not in VALID_MODELS:
            raise ConfigError(
                f"Invalid model '{model}'. Must be one of: {', '.join(VALID_MODELS)}"
            )
        logger.debug("Overriding model to '%s'", model)
        cfg.model.name = model

    if device is not None:
        if device not in VALID_DEVICES:
            raise ConfigError(
                f"Invalid device '{device}'. Must be one of: {', '.join(VALID_DEVICES)}"
            )
        logger.debug("Overriding compute device to '%s'", device)
        cfg.model.device = device

    if input_device is not None:
        logger.debug("Overriding input device to '%s'", input_device)
        cfg.input.device = input_device

    if dry_run:
        logger.debug("Enabling dry-run mode")
        cfg.injector.dry_run = True

    return cfg


@app.command()
def run(
    config: Path | None = typer.Option(
        None, "--config", help="Path to configuration file"
    ),
    verbose: bool = typer.Option(
        False, "--verbose", "-v", help="Enable verbose logging"
    ),
    audio_device: int | None = typer.Option(
        None, "--audio-device", "-a", help="Override audio device by index"
    ),
    model: str | None = typer.Option(
        None, "--model", "-m", help="Override model (tiny, base, small, medium, large)"
    ),
    device: str | None = typer.Option(
        None, "--device", help="Override compute device (cpu, cuda, auto)"
    ),
    input_device: str | None = typer.Option(
        None, "--input-device", help="Override input device path"
    ),
    dry_run: bool = typer.Option(
        False, "--dry-run", help="Run in dry-run mode without text injection"
    ),
) -> None:
    """Run the dictation helper daemon."""
    _setup_logging(verbose)
    try:
        cfg = load_config(config)
        logger.info("Loaded config from: %s", config or "default locations")
        logger.debug("Config: %s", cfg)

        cfg = _merge_config_overrides(
            cfg,
            audio_device=audio_device,
            model=model,
            device=device,
            input_device=input_device,
            dry_run=dry_run,
        )
        cfg.validate()
        logger.info("Configuration validated successfully")

        # Initialize components
        button_listener = ButtonListener.from_config(cfg.input)
        recorder = AudioRecorder(
            sample_rate=cfg.audio.sample_rate,
            channels=cfg.audio.channels,
            chunk_size=cfg.audio.chunk_size,
            trim_silence=True,
            device=cfg.audio.device,
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
    audio_device: int | None = typer.Option(
        None, "--audio-device", "-a", help="Override audio device by index"
    ),
    model: str | None = typer.Option(
        None, "--model", "-m", help="Override model (tiny, base, small, medium, large)"
    ),
    device: str | None = typer.Option(
        None, "--device", help="Override compute device (cpu, cuda, auto)"
    ),
    input_device: str | None = typer.Option(
        None, "--input-device", help="Override input device path"
    ),
) -> None:
    """Run a dry-run without injection (for testing)."""
    _setup_logging(verbose)
    try:
        cfg = load_config(config)
        logger.info("Loaded config from: %s", config or "default locations")
        cfg = _merge_config_overrides(
            cfg,
            audio_device=audio_device,
            model=model,
            device=device,
            input_device=input_device,
            dry_run=True,
        )
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
