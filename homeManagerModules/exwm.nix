{ config, lib, osConfig, pkgs, ... }: let
  cfg = config.thoughtfull.exwm;
in {
  options.thoughtfull.exwm.enable = lib.mkOption {
    default = config.thoughtfull.desktop.enable;
    description = "Whether to enable exwm.";
    type = lib.types.bool;
  };
  config = lib.mkIf cfg.enable {
    home = {
      packages = [ pkgs.thoughtfull.exwm-trampoline ];
      sessionVariables.EDITOR = "emacsclient";
    };
    programs.emacs = {
      enable = true;
      extraConfig = "(require 'tfl-exwm)";
      extraPackages = epkgs: [ epkgs.tfl-exwm ];
    };
    xsession = {
      enable = true;
      initExtra = lib.mkIf (!osConfig.services.xserver.desktopManager.xfce.enable)
        (lib.mkAfter "[ ! -f $\{HOME}/.noexwm ] && exwm-trampoline &");
    };
  };
}
