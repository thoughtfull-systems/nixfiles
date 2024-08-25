{ config, lib, ... }: let
  cfg = config.thoughtfull.emacs.gtd;
in {
  options.thoughtfull.emacs.gtd.enable = lib.mkEnableOption "gtd";
  config = lib.mkIf cfg.enable {
    programs.emacs = {
      extraConfig = "(require 'tfl-gtd)";
      extraPackages = epkgs: [ epkgs.tfl-gtd ];
    };
  };
}
