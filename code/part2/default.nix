{ reflex-platform ? import ./reflex-platform.nix
, compiler ? "ghcjs"
} :
let
  pkgs = reflex-platform.nixpkgs.pkgs;

  haskellPackages = reflex-platform.${compiler}.override {
    overrides = (self: super: {
      ghc = super.ghc // { withPackages = super.ghc.withHoogle; };
      ghcWithPackages = self.ghc.withPackages;
      common = pkgs.haskell.lib.dontHaddock (
        import ../common { inherit compiler; }
      );
      reflex-dom-datepicker = pkgs.haskell.lib.dontHaddock (
        import ./pkgs/reflex-dom-datepicker.nix
      );
    });
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
      cp ${haskellPackages.reflex-dom-datepicker}/css/* $out/css/reflex/growing-a-datepicker/

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
