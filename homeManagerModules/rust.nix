{ config, lib, pkgs, ... } : let
  cfg = config.thoughtfull.rust;
in {
  options.thoughtfull.rust.enable = lib.mkEnableOption "rust";
  config = lib.mkIf cfg.enable {
    home = {
      packages = with pkgs; [
        cargo
        clippy
        gcc
        rust-analyzer
        rustc
        rustfmt
        rusty-man
      ];
      sessionVariables = {
        PATH = "$PATH:$HOME/.cargo/bin";
      };
    };
    programs.emacs = {
      extraConfig = "(require 'tfl-rust)";
      extraPackages = epkgs: [ epkgs.tfl-rust ];
    };
  };
}
