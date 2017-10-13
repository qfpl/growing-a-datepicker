let
  initialNixpkgs = import <nixpkgs> {};

  sources = {
    reflex-dom-datepicker = initialNixpkgs.pkgs.fetchFromGitHub {
      owner  = "qfpl";
      repo   = "reflex-dom-datepicker";
      rev    = "7a2784047a2531f97b1d0585f14f9afc50647808";
      sha256 = "1l1qkkh5b26a9f42rdhr69yx7136gb9ymv61jxb079yi1j5r2ky5";
    };
  };

  reflex-dom-datepicker = import sources.reflex-dom-datepicker {};
in
  reflex-dom-datepicker
