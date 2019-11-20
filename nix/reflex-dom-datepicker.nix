let
  reflex-dom-datepickerJson =
    builtins.fromJSON (builtins.readFile ./reflex-dom-datepicker.json);

  reflex-dom-datepicker =
    builtins.fetchTarball (with reflex-dom-datepickerJson; {
      url = "${url}/archive/${rev}.tar.gz";
      inherit sha256;
    });
in
  reflex-dom-datepicker
