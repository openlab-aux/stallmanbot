let
  nixpkgs = import <nixpkgs> {};
in
nixpkgs.haskellPackages.callPackage ./stallmanbot.nix {}
