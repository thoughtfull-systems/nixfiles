{ config, lib, pkgs, ... } : let
  cfg = config.thoughtfull.rust;
in {
  options.thoughtfull.rust.enable = lib.mkEnableOption "rust";
  config = lib.mkIf cfg.enable {
    home.packages = with pkgs; [
      cargo
      gcc
      rustc
      rustfmt
      rusty-man
    ];
    programs.emacs = {
      extraConfig = "(require 'tfl-rust)";
      extraPackages = epkgs: [ epkgs.tfl-rust ];
    };
  };
}
