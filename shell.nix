# A very simple shell.nix file for setting up necessary build tools. This is
# likely going to be updated using the iohk-specific nixpkgs and a haskel.nix
# derivation of our cabal.project.
{ compiler ? "ghc8104"
  # Latest haskell.nix for more likely cache hits
, haskellNix ? import
    (builtins.fetchTarball
      "https://github.com/input-output-hk/haskell.nix/archive/abb289ff961fb1bd98eb0340088c432f15109807.tar.gz")
    { }
  # Use same pkgs as haskell.nix for more likely cache hits
, nixpkgsSrc ? haskellNix.sources.nixpkgs-2009
, nixpkgsArgs ? haskellNix.nixpkgsArgs
, pkgs ? import nixpkgsSrc nixpkgsArgs
  # Use cardano-node master for more likely cache hits
}:
with pkgs;
let
  hls = haskell-nix.tool compiler "haskell-language-server" "latest";
  ghc = haskell-nix.compiler.${compiler};
  fourmolu = haskell-nix.tool compiler "fourmolu" "latest";
  cabal-plan = pkgs.haskellPackages.cabal-plan ;
  ghcid = pkgs.haskellPackages.ghcid ;
  hspec-discover = pkgs.haskellPackages.hspec-discover ;
  libsodium-vrf = libsodium.overrideAttrs (oldAttrs: {
    name = "libsodium-1.0.18-vrf";
    src = fetchFromGitHub {
      owner = "input-output-hk";
      repo = "libsodium";
      # branch tdammers/rebased-vrf
      rev = "66f017f16633f2060db25e17c170c2afa0f2a8a1";
      sha256 = "12g2wz3gyi69d87nipzqnq4xc6nky3xbmi2i2pb2hflddq8ck72f";
    };
    nativeBuildInputs = [ autoreconfHook ];
    configureFlags = "--enable-static";
  });
in
mkShell rec {
  name = "hydra-sim-env";

  tools = [
    cabal-install
    fourmolu
    ghcid
    ghc
    hls
    cabal-plan
    # needed for HLS to work properly, see https://github.com/haskell/haskell-language-server/issues/176
    hspec-discover
    # For scripts/
    nodejs-14_x
    # For discovering libs (below)
    pkgconfig
  ];

  libs = [
    libsodium-vrf
    zlib
  ];

  buildInputs = tools ++ libs;

  # Ensure that libz.so and other libraries are available to TH splices.
  LD_LIBRARY_PATH = lib.makeLibraryPath libs;

  # Force a UTF-8 locale because many Haskell programs and tests
  # assume this.
  LANG = "en_US.UTF-8";
}
