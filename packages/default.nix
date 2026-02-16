inputs:
inputs.self.lib.forAllSystems (system: let
  nixpkgs = import inputs.nixpkgs {
    config.allowUnfree = true;
    inherit system;
  };
in {
  mic = import ./mic nixpkgs;
  yubikey-touch-plugin = import ./yubikey-touch-plugin nixpkgs;
  zoom-us = import ./zoom-us.nix nixpkgs;
})
