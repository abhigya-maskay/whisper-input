{
  description = "whisper-input: A wrapper around the OpenAI Whisper model for dictation";

  inputs = {
    nixpkgs.url = "github:NixOS/nixpkgs/nixpkgs-unstable";
  };

  outputs = { self, nixpkgs }:
    let
      system = "x86_64-linux";
      pkgs = nixpkgs.legacyPackages.${system};
    in
    {
      packages.${system}.default = pkgs.haskell.packages.ghc912.callCabal2nix "whisper-input" ./. { };

      devShells.${system}.default = pkgs.mkShell {
        buildInputs = [
          pkgs.haskell.packages.ghc912.ghc
          pkgs.cabal-install
          pkgs.nixpkgs-fmt
          pkgs.haskell.packages.ghc912.haskell-language-server
          pkgs.haskell.packages.ghc912.ormolu
          pkgs.haskell.packages.ghc912.hlint
        ];

        shellHook = ''
          # Configure git to use .githooks directory for hooks
          if git rev-parse --git-dir > /dev/null 2>&1; then
            git config --local core.hooksPath .githooks
            echo "Git hooks path set to .githooks"
          fi
        '';
      };
    };
}
