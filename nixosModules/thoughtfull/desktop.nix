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
        displayManager = {
          autoLogin = {
            enable = lib.mkDefault true;
            user = lib.mkIf (config.users.users ? paul) (lib.mkDefault "paul");
          };
          lightdm.enable = lib.mkDefault true;
        };
        enable = lib.mkDefault true;
      };
    };
    time.timeZone = lib.mkDefault "America/New_York";
  };
}
