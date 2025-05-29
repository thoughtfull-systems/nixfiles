{ config, lib, pkgs, ... }: let
  desktop = config.thoughtfull.desktop.enable;
in {
  options.thoughtfull.desktop.enable = lib.mkEnableOption "desktop";
  config = lib.mkIf desktop {
    boot.loader.timeout = lib.mkDefault 5;
    environment.defaultPackages = with pkgs.thoughtfull; [ pins uns ];
    home-manager.sharedModules = [({ ... }: {
      thoughtfull.desktop.enable = lib.mkDefault true;
    })];
    networking.networkmanager.enable = lib.mkDefault true;
    security.rtkit.enable = lib.mkDefault config.services.pulseaudio.enable;
    services = {
      pipewire = {
        extraConfig.pipewire = {
          "99-disable-bell" = {
            "context.properties"= {
              "module.x11.bell" = false;
            };
          };
        };
        pulse.enable = lib.mkDefault true;
      };
      printing.enable = lib.mkDefault true;
      xserver = {
        desktopManager.xfce.enable = lib.mkDefault true;
        displayManager.lightdm = {
          enable = lib.mkDefault true;
          greeters.gtk.indicators = [
            "~host"
            "~spacer"
            "~clock"
            "~spacer"
            "~session"
            "~power"
          ];
        };
        enable = lib.mkDefault true;
      };
    };
    time.timeZone = lib.mkDefault "America/New_York";
  };
}
