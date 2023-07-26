{ config, lib, pkgs, ... }:
lib.mkIf config.services.xserver.desktopManager.xfce.enable {
  nixpkgs.overlays = [
    (self: super: {
      xfce = super.xfce // {
        xfce4-power-manager = super.xfce.xfce4-power-manager.overrideAttrs (
          (prevAttrs: {
            patches = [ ./0001-Hybrid-Sleep-v2.2.patch ];
          }));
      };
    })
  ];
  environment = {
    systemPackages = with pkgs.xfce; [
      xfce4-panel
      xfce4-pulseaudio-plugin
      xfce4-xkb-plugin
      xfce4-weather-plugin
    ];
    xfce.excludePackages = [ pkgs.xfce.xfce4-volumed-pulse ];
  };
  programs.thunar.plugins = with pkgs.xfce; [
    thunar-archive-plugin
    thunar-media-tags-plugin
    thunar-volman
  ];
  services = {
    xserver = {
      desktopManager.xfce = {
        noDesktop = lib.mkDefault true;
        enableScreensaver = lib.mkDefault false;
        enableXfwm = lib.mkDefault false;
      };
      displayManager.lightdm.enable = true;
      libinput.touchpad.tapping = false;
    };
    logind.lidSwitch = "ignore";
  };
}
