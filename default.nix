{ pkgs ? import <nixpkgs> {}
, hpkgs ? pkgs.haskellPackages
, mkDerivation ? expr: hpkgs.mkDerivation (expr // {
    enableSeparateDocOutput = true;
    doHaddock = true;
  })
, polytime ? hpkgs.callPackage ../polytime { inherit pkgs hpkgs mkDerivation; }
}: hpkgs.callCabal2nix (builtins.baseNameOf ./.) (pkgs.lib.cleanSource ./.) { inherit mkDerivation polytime; }
