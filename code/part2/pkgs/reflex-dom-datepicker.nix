let
  initialNixpkgs = import <nixpkgs> {};

  sources = {
    reflex-dom-datepicker = initialNixpkgs.pkgs.fetchFromGitHub {
      owner  = "qfpl";
      repo   = "reflex-dom-datepicker";
      rev    = "7f3ea42065ffb48bfa593af3271f74bf1614dd73";
      sha256 = "1cva87d2467plz7kgrkknc2aqx6n6wm4y7nf7l3nxh9aa01chmh7";
      # rev    = "205ae0f9efdb37076c0f1e5e8f0e35d8bcea7bf8";
      # sha256 = "074mijx46v5xn9j9zb15g0gaycry8wardfxh6n1hzcpz3sa9wis7";
    };
  };

  reflex-dom-datepicker = import sources.reflex-dom-datepicker {};
in
  reflex-dom-datepicker

