let
  drv = import ./default.nix { compiler = "ghcjs"; };
in
  drv.env
