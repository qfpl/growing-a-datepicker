let
  initialNixpkgs = import <nixpkgs> {};

  sources = {
    reflex-dom-datepicker = initialNixpkgs.pkgs.fetchFromGitHub {
      owner  = "qfpl";
      repo   = "reflex-dom-datepicker";
      rev    = "2a800bc6ca4abe2ca1d7365c7d64e62290468192";
      sha256 = "10l9q6dhnwqlz4iffmw2i1fsd50ay2vrzhzqwg3v8rpjqfza79lx";
    };
  };

  reflex-dom-datepicker = import sources.reflex-dom-datepicker {};
in
  reflex-dom-datepicker
