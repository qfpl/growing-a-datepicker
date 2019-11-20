#!/usr/bin/env nix-shell
#!nix-shell -i bash -p nix-prefetch-git

nix-prefetch-git https://github.com/NixOS/nixpkgs > nixpkgs.json
nix-prefetch-git https://github.com/reflex-frp/reflex-platform > reflex-platform.json
nix-prefetch-git https://github.com/qfpl/reflex-dom-datepicker > reflex-dom-datepicker.json
