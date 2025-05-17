{ config, lib, pkgs, ... } : let
  cfg = config.thoughtfull.aider;
in {
  options.thoughtfull.aider = {
    enable = lib.mkEnableOption "aider";
  };
  config = lib.mkIf cfg.enable {
    home.packages = with pkgs; [ aider-chat ];
    programs.emacs = {
      extraConfig = "(require 'tfl-aider)";
      extraPackages = epkgs: [ epkgs.tfl-aider ];
    };
  };
}
