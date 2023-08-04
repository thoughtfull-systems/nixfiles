{ config, lib, ... } : let
  cfg = config.services.openssh;
in {
  config.services.openssh = lib.mkMerge [
    {
      enable = lib.mkDefault (!config.thoughtfull.desktop.enable);
    }
    (lib.mkIf cfg.enable {
      settings.PasswordAuthentication = lib.mkDefault false;
    })
  ];
}
