{ mkDerivation, base, common, ghcjs-dom, jsaddle, lens, reflex
, reflex-dom, reflex-dom-datepicker, stdenv, text, time
}:
mkDerivation {
  pname = "part2";
  version = "0.1.0.0";
  src = ./.;
  isLibrary = true;
  isExecutable = true;
  libraryHaskellDepends = [
    base common ghcjs-dom jsaddle lens reflex reflex-dom
    reflex-dom-datepicker text time
  ];
  executableHaskellDepends = [ base ];
  doHaddock = false;
  license = stdenv.lib.licenses.bsd3;
}
