# Remaining Organization Gaps

- **Daemon Lifecycle Coupling** *(WhisperInput.Commands:44)*
  Daemon bootstrap (PID locks, signal handlers, cleanup) lives with CLI client dispatch. Split into a dedicated daemon runner module and a lean client/CLI module to clarify ownership and improve reuse/testing.

- **PidFile Naming Mismatch** *(WhisperInput.PidFile:59)*  
  Module name suggests PID-only duties, yet it also removes sockets/directories. Either rename to reflect the broader role (e.g. `SocketLock`) or move the socket cleanup helpers beside the server startup code for clearer discovery.
