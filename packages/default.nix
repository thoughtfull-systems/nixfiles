inputs:
inputs.self.lib.forAllSystems (system: let
  nixpkgs = import inputs.nixpkgs {
    config.allowUnfree = true;
    inherit system;
  };
in {
  brightness = import ./brightness nixpkgs;
  exwm-trampoline = import ./exwm-trampoline nixpkgs;
  keyboard = import ./keyboard nixpkgs;
  mic = import ./mic nixpkgs;
  pins = import ./pins nixpkgs;
  speaker = import ./speaker nixpkgs;
  uns = import ./uns nixpkgs;
  yubikey-touch-plugin = import ./yubikey-touch-plugin nixpkgs;
  zoom-us = import ./zoom-us.nix nixpkgs;
})
