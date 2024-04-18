{ config, lib, pkgs, ... }: let
  cfg = config.thoughtfull.desktop.obsidian;
in {
  options.thoughtfull.desktop.obsidian.enable = lib.mkOption {
    default = config.thoughtfull.desktop.enable;
    example = false;
    description = "Whether to enable obsidian.";
    type = lib.types.bool;
  };
  config = lib.mkIf cfg.enable {
    home.packages = [ pkgs.unstable.obsidian ];
    programs.emacs = {
      extraConfig = "(require 'tfl-ol-obsidian)";
      extraPackages = epkgs: [ epkgs.tfl-ol-obsidian ];
    };
  };
}
