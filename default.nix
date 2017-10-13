let
  pkgs = import <nixpkgs> {};
  blog = import ./blog {};
  part2 = import ./code/part2 {};
in
  pkgs.stdenv.mkDerivation rec {
    name = "growing-a-datepicker";
    src = ./.;
    installPhase = ''
      mkdir -p $out;

      ln -sv ${blog}/drafts $out/drafts
      ln -sv ${blog}/posts $out/posts

      mkdir -p $out/js/reflex
      ln -sv ${part2}/js/reflex/growing-a-datepicker $out/js/reflex/growing-a-datepicker

      mkdir -p $out/css/reflex
      ln -sv ${part2}/css/reflex/growing-a-datepicker $out/css/reflex/growing-a-datepicker
    '';
    phases = ["installPhase"];
  }
