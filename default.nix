{ pkgs ? import <nixpkgs> {}
, hpkgs ? pkgs.haskellPackages
, mkDerivation ? expr: hpkgs.mkDerivation (expr // {
    enableSeparateDocOutput = true;
    doHaddock = true;
  })
}: hpkgs.callCabal2nix (builtins.baseNameOf ./.) (pkgs.lib.cleanSource ./.) { inherit mkDerivation; }
