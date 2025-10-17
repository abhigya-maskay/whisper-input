{
  description = "Hyprland dictation helper – reproducible dev & build env";

  inputs = {
    nixpkgs.url = "github:nixos/nixpkgs/nixos-unstable";
    flake-utils.url = "github:numtide/flake-utils";
  };

  outputs = { self, nixpkgs, flake-utils }:
    flake-utils.lib.eachDefaultSystem (system:
      let
        pkgs = import nixpkgs {
          inherit system;
          config = {
            allowUnfree = true;
            cudaSupport = true;
          };
          overlays = [
            (final: prev: {
              ctranslate2 = prev.ctranslate2.override {
                withCUDA = true;
                withCuDNN = true;
              };

              python312 = prev.python312.override {
                packageOverrides = self: super: {
                  ctranslate2 = super.ctranslate2.overridePythonAttrs (old: {
                    doCheck = false;
                    nativeCheckInputs = [];
                  });

                  faster-whisper = super.faster-whisper.overridePythonAttrs (old: {
                    propagatedBuildInputs = (old.propagatedBuildInputs or []) ++ [ self.ctranslate2 ];
                  });
                };
              };
            })
          ];
        };
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
            soundfile
            scipy
            evdev
            typer
            onnxruntime
            tomli
          ];
        };

        packages.default = self.packages.${system}.dictation-app;

        checks.${system} = {
          test = pkgs.stdenv.mkDerivation {
            name = "dictation-app-tests";
            src = ./.;
            buildInputs = with pythonPkgs; [
              pytest
              pytest-asyncio
              faster-whisper
              numpy
              sounddevice
              soundfile
              scipy
              evdev
              typer
              onnxruntime
              tomli
            ];
            buildPhase = ''
              python -m pytest tests/ -v --tb=short
            '';
            installPhase = "mkdir -p $out";
          };

          lint = pkgs.stdenv.mkDerivation {
            name = "dictation-app-lint";
            src = ./.;
            buildInputs = with pythonPkgs; [
              ruff
              black
            ];
            buildPhase = ''
              ruff check dictation_app tests
              black --check dictation_app tests
            '';
            installPhase = "mkdir -p $out";
          };
        };

        devShells.default = pkgs.mkShell {
          nativeBuildInputs = with pkgs; [
            cudaPackages.cudatoolkit
            cudaPackages.cudnn
          ];
          buildInputs = with pkgs; [
            (python.withPackages (ps: with ps; [
              faster-whisper
              numpy
              sounddevice
              soundfile
              scipy
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
            pre-commit
          ];

          LD_LIBRARY_PATH = pkgs.lib.makeLibraryPath [
            pkgs.cudaPackages.cudatoolkit
            pkgs.cudaPackages.cudnn
          ];

          shellHook = ''
            echo "Dictation Helper dev environment loaded"
            echo "Python: $(python --version)"
            export CUDA_PATH=${pkgs.cudaPackages.cudatoolkit}
            export CUDA_HOME=${pkgs.cudaPackages.cudatoolkit}
            export LD_LIBRARY_PATH=${pkgs.lib.makeLibraryPath [ pkgs.cudaPackages.cudatoolkit pkgs.cudaPackages.cudnn ]}:$LD_LIBRARY_PATH
            pre-commit install --install-hooks > /dev/null 2>&1 || true
          '';
        };
      }
    );
}
