{ config, lib, pkgs, ... }:
lib.mkIf config.services.xserver.desktopManager.xfce.enable {
  nixpkgs.overlays = [
    (self: super: {
      xfce = super.xfce // {
        xfce4-pulseaudio-plugin = super.xfce.xfce4-pulseaudio-plugin.overrideAttrs (
          (prevAttrs: {
            buildInputs = prevAttrs.buildInputs ++ [
              super.libcanberra
            ];
          }));
        xfce4-power-manager = super.xfce.xfce4-power-manager.overrideAttrs (
          (prevAttrs: {
            patches = [ ./0001-Hybrid-Sleep-v2.2.patch ];
            # this is a temporary hack to fix #45, but something better should e done
            postPatch = ''
              substituteInPlace src/org.xfce.power.policy.in2 --replace "@sbindir@" "/run/current-system/sw/bin"
              substituteInPlace common/xfpm-brightness.c --replace "SBINDIR" "\"/run/current-system/sw/bin\""
              substituteInPlace src/xfpm-suspend.c --replace "SBINDIR" "\"/run/current-system/sw/bin\""
            '';
          }));
      };
    })
  ];
  environment.systemPackages = with pkgs; [
    xfce.xfce4-panel
    xfce.xfce4-pulseaudio-plugin
    xfce.xfce4-xkb-plugin
    xfce.xfce4-weather-plugin
  ];
  services = {
    xserver = {
      desktopManager.xfce = {
        noDesktop = lib.mkDefault true;
        enableScreensaver = lib.mkDefault false;
        enableXfwm = lib.mkDefault false;
        excludePackages = [ pkgs.xfce.xfce4-volumed-pulse ];
      };
      displayManager.lightdm.enable = true;
    };
    logind.lidSwitch = "ignore";
  };
}
