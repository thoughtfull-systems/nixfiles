{ config, lib, pkgs, ... }: let
  xfce = config.services.xserver.desktopManager.xfce.enable;
in lib.mkIf xfce {
  environment = {
    systemPackages = with pkgs.xfce; [
      xfce4-genmon-plugin
      xfce4-panel
      xfce4-pulseaudio-plugin
      xfce4-xkb-plugin
      xfce4-weather-plugin
    ] ++ [
      pkgs.gnome.file-roller
    ];
    xfce.excludePackages = [ pkgs.xfce.xfce4-volumed-pulse ];
  };
  programs.thunar.plugins = with pkgs.xfce; [
    thunar-archive-plugin
    thunar-media-tags-plugin
    thunar-volman
  ];
  services = {
    libinput.touchpad.tapping = lib.mkDefault false;
    logind.lidSwitch = lib.mkDefault "ignore";
    xserver = {
      desktopManager.xfce = {
        enableScreensaver = lib.mkDefault true;
        enableXfwm = lib.mkDefault true;
        noDesktop = lib.mkDefault true;
      };
      displayManager.lightdm.enable = lib.mkDefault true;
    };
  };
}
