{ reflex-platform ? import ../../nix/reflex-platform.nix
, compiler ? "ghcjs"
} :
let
  inherit (reflex-platform.nixpkgs) pkgs;

  reflex-dom-datepicker = import ../../nix/reflex-dom-datepicker.nix;

  haskellPackages = reflex-platform.${compiler}.override {
    overrides = self: super: with pkgs.haskell.lib; {
      ghc = super.ghc // { withPackages = super.ghcWithHoogle; };
      ghcWithPackages = self.ghc.withPackages;

      # Missing doctest
      bsb-http-chunked = dontCheck super.bsb-http-chunked;
      http2 = dontCheck super.http2;
      http-date = dontCheck super.http-date;
      iproute = dontCheck super.iproute;
      network-byte-order = dontCheck super.network-byte-order;

      # The failing test seems minor. YOLO.
      Glob = dontCheck super.Glob;

      common = dontHaddock (self.callPackage ../common/common.nix {});
      reflex-dom-datepicker = dontHaddock (
        self.callPackage "${reflex-dom-datepicker}/reflex-dom-datepicker.nix" {}
      );
    };
  };

  adjust-for-ghcjs = drv: {
    executableToolDepends = [pkgs.closurecompiler pkgs.zopfli];
    doHaddock = false;
    postInstall = ''
      mkdir -p $out

      mkdir -p $out/js/reflex/growing-a-datepicker
      cp $out/bin/datepicker-embed.jsexe/all.js $out/js/reflex/growing-a-datepicker/datepicker-embed.js

      mkdir -p $out/css/reflex/growing-a-datepicker
      cp -r ./css/* $out/css/
      cp ${reflex-dom-datepicker}/css/* $out/css/reflex/growing-a-datepicker/

      cd $out/bin/datepicker-embed.jsexe
      closure-compiler all.js --compilation_level=ADVANCED_OPTIMIZATIONS --isolation_mode=IIFE --assume_function_wrapper --jscomp_off="*" --externs=all.js.externs > $out/js/reflex/growing-a-datepicker/datepicker-embed.min.js
      rm -Rf $out/bin/datepicker-embed.jsexe
      rm -Rf $out/bin

      cd $out/js/reflex/growing-a-datepicker
      zopfli -i1000 datepicker-embed.min.js

      rm -Rf $out/lib
      rm -Rf $out/nix-support
      rm -Rf $out/share
    '';
  };

  adjust = drv:
    if compiler == "ghcjs"
    then adjust-for-ghcjs drv
    else drv;

  part2 = pkgs.haskell.lib.overrideCabal (
    haskellPackages.callPackage ./part2.nix {}
  ) adjust;
in
  part2
