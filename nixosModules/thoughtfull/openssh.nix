{ config, lib, ... } : let
  desktop = config.thoughtfull.desktop.enable;
  cfg = config.services.openssh;
in {
  config = {
    services.openssh = {
      enable = lib.mkOverride 900 (!desktop);
      settings.PasswordAuthentication = lib.mkIf cfg.enable (lib.mkDefault false);
    };
    thoughtfull.systemd-notify-failure.services = lib.mkIf cfg.enable [ "sshd" ];
  };
}
