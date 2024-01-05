{ config, lib, ... }: {
  options.thoughtfull.emacs.gtd.enable = lib.mkEnableOption "gtd";
  config = lib.mkIf config.thoughtfull.emacs.gtd.enable {
    programs.emacs = {
      extraConfig = "(require 'tfl-gtd)";
      extraPackages = epkgs: [ epkgs.tfl-gtd ];
    };
  };
}
