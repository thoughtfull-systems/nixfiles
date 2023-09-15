{ config, lib, osConfig, pkgs, ... }: let
  enable = osConfig.services.xserver.desktopManager.xfce.enable;
  xfconf-args = {
    inherit config pkgs;
    uint = value: { type = "uint"; value = value; };
  };
in lib.mkIf enable {
  services.picom.enable = true;
  xfconf.settings = {
    accessibility = import ./xfconf/accessibility.nix xfconf-args;
    keyboard-layout = import ./xfconf/keyboard-layout.nix xfconf-args;
    keyboards = import ./xfconf/keyboards.nix xfconf-args;
    pointers = import ./xfconf/pointers.nix xfconf-args;
    thunar = import ./xfconf/thunar.nix xfconf-args;
    xfce4-keyboard-shortcuts = import ./xfconf/xfce4-keyboard-shortcuts.nix xfconf-args;
    xfce4-notifyd = import ./xfconf/xfce4-notifyd.nix xfconf-args;
    xfce4-panel = import ./xfconf/xfce4-panel.nix xfconf-args;
    xfce4-power-manager = import ./xfconf/xfce4-power-manager.nix xfconf-args;
    xfce4-session = import ./xfconf/xfce4-session.nix xfconf-args;
    xsettings = import ./xfconf/xsettings.nix xfconf-args;
  };
}
