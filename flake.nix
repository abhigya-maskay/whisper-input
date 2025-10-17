{
  description = "Hyprland dictation helper – reproducible dev & build env";

  inputs = {
    nixpkgs.url = "github:nixos/nixpkgs/nixos-unstable";
    flake-utils.url = "github:numtide/flake-utils";
  };

  outputs = { self, nixpkgs, flake-utils }:
    flake-utils.lib.eachDefaultSystem (system:
      let
        pkgs = import nixpkgs { inherit system; };
        python = pkgs.python312;
        pythonPkgs = python.pkgs;
      in
      {
        packages.dictation-app = pythonPkgs.buildPythonApplication {
          pname = "dictation-app";
          version = "0.1.0";
          src = ./.;
          pyproject = true;

          nativeBuildInputs = with pythonPkgs; [
            hatchling
          ];

          propagatedBuildInputs = with pythonPkgs; [
            faster-whisper
            numpy
            sounddevice
            evdev
            typer
            onnxruntime
            tomli
          ];
        };

        packages.default = self.packages.${system}.dictation-app;

        devShells.default = pkgs.mkShell {
          buildInputs = with pkgs; [
            (python.withPackages (ps: with ps; [
              faster-whisper
              numpy
              sounddevice
              evdev
              typer
              onnxruntime
              tomli
              pytest
              pytest-asyncio
              hatchling
              pip
            ]))
            # Wayland utilities
            wtype
            wl-clipboard
            ydotool
            sox
            pipewire
            libnotify
            # Dev tools
            ruff
            black
          ];

          shellHook = ''
            echo "Dictation Helper dev environment loaded"
            echo "Python: $(python --version)"
          '';
        };
      }
    );
}
