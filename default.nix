{ # Fetch the latest haskell.nix and import its default.nix
  haskellNix ?
  import
    (builtins.fetchTarball
      "https://github.com/input-output-hk/haskell.nix/archive/abb289ff961fb1bd98eb0340088c432f15109807.tar.gz")
    {}

# haskell.nix provides access to the nixpkgs pins which are used by our CI,
# hence you will be more likely to get cache hits when using these.
# But you can also just use your own, e.g. '<nixpkgs>'.
, nixpkgsSrc ? haskellNix.sources.nixpkgs

# haskell.nix provides some arguments to be passed to nixpkgs, including some
# patches and also the haskell.nix functionality itself as an overlay.
, nixpkgsArgs ? haskellNix.nixpkgsArgs

# import nixpkgs with overlays
, pkgs ? import nixpkgsSrc nixpkgsArgs
}: pkgs.haskell-nix.project {
  # 'cleanGit' cleans a source directory based on the files known by git
  src = pkgs.haskell-nix.haskellLib.cleanGit {
    name = "hydra-sim";
    src = ./.;
  };
  # Specify the GHC version to use.
  compiler-nix-name = "ghc8104"; # Not required for `stack.yaml` based projects.
}
