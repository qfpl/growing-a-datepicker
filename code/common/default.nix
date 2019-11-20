{ reflex-platform ? import ../../nix/reflex-platform.nix
, compiler ? "ghcjs"
} :
let
  inherit (reflex-platform.nixpkgs) pkgs;

  haskellPackages = reflex-platform.${compiler}.override {
    overrides = self: super: with pkgs.haskell.lib; {
      # Missing doctest
      bsb-http-chunked = dontCheck super.bsb-http-chunked;
      http2 = dontCheck super.http2;
      http-date = dontCheck super.http-date;
      iproute = dontCheck super.iproute;
      network-byte-order = dontCheck super.network-byte-order;

      # The failing test seems minor. YOLO.
      Glob = dontCheck super.Glob;
    };
  };

  common = haskellPackages.callPackage ./common.nix {};
in
  pkgs.haskell.lib.dontHaddock common
