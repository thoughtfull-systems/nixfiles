{ config, lib, ... }: let
  cfg = config.thoughtfull.desktop;
in {
  options.thoughtfull.desktop.enable = lib.mkEnableOption "desktop";
  config = lib.mkIf cfg.enable {
    hardware.pulseaudio.enable = lib.mkDefault true;
    home-manager.sharedModules = [({ ... }: {
      thoughtfull.desktop.enable = lib.mkDefault true;
    })];
    networking.networkmanager.enable = lib.mkDefault true;
    security.rtkit.enable = lib.mkDefault config.hardware.pulseaudio.enable;
    services = {
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
            "~layout"
            "~power"
          ];
        };
        enable = lib.mkDefault true;
      };
    };
    time.timeZone = lib.mkDefault "America/New_York";
  };
}
