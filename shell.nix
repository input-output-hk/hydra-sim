{ compiler ? "ghc8107"
, system ? builtins.currentSystem
, pkgs ? import (builtins.fetchTarball {
    url = "https://github.com/NixOS/nixpkgs/archive/97885bb7dd36b559c511c20d13e13affb9fef678.tar.gz";
  }) {}
}:
pkgs.mkShell rec {
  name = "hydra-sim-env";

  tools = [
    pkgs.cabal-install
    pkgs.haskellPackages.fourmolu
    pkgs.ghcid
    pkgs.ghc
    pkgs.haskell-language-server
    pkgs.haskellPackages.cabal-plan
    # needed for HLS to work properly, see https://github.com/haskell/haskell-language-server/issues/176
    pkgs.haskellPackages.hspec-discover
    # For datasets/
    pkgs.nodejs-16_x
    # For discovering libs (below)
    pkgs.pkgconfig
  ];

  libs = [
    pkgs.zlib
    # Used by charting library in scripts/
    pkgs.libuuid
  ];

  buildInputs = tools ++ libs;

  # Ensure that libz.so and other libraries are available to TH splices.
  LD_LIBRARY_PATH = pkgs.lib.makeLibraryPath libs;

  # Force a UTF-8 locale because many Haskell programs and tests
  # assume this.
  LANG = "en_US.UTF-8";
}
