{ compiler ? "ghc8107" }:

let
  sources = import ./nix/sources.nix;
  pkgs = import sources.nixpkgs {};

  gitignore = pkgs.nix-gitignore.gitignoreSourcePure [ ./.gitignore ];

  myHaskellPackages = pkgs.haskell.packages.${compiler}.override {
    overrides = hself: hsuper: {
      "hlsexamples" =
        hself.callCabal2nix
          "hlsexamples"
          (gitignore ./.)
          {};
    };
  };

  shell = myHaskellPackages.shellFor {
    packages = p: [
      p."hlsexamples"
    ];
    buildInputs = [
      pkgs.haskellPackages.cabal-install
      pkgs.haskellPackages.cabal-fmt
      pkgs.haskellPackages.ghcid
      pkgs.haskellPackages.haskell-language-server
      pkgs.haskellPackages.hasktags
      pkgs.haskellPackages.hlint
      pkgs.haskellPackages.ormolu
      pkgs.haskellPackages.stylish-haskell
      pkgs.niv
      pkgs.nixpkgs-fmt
    ];
    withHoogle = true;
  };

  exe = pkgs.haskell.lib.justStaticExecutables (myHaskellPackages."hlsexamples");

  docker = pkgs.dockerTools.buildImage {
    name = "hlsexamples";
    config.Cmd = [ "${exe}/bin/hlsexamples" ];
  };
in
{
  inherit shell;
  inherit exe;
  inherit docker;
  inherit myHaskellPackages;
  "hlsexamples" = myHaskellPackages."hlsexamples";
}
