{ config, lib, ... } : let
  desktop = config.thoughtfull.desktop.enable;
  openssh = config.services.openssh.enable;
in {
  config = {
    services.openssh = {
      enable = lib.mkOverride 900 (!desktop);
      settings.PasswordAuthentication = lib.mkIf openssh (lib.mkDefault false);
    };
    thoughtfull.systemd-notify-failure.services = lib.mkIf openssh [ "sshd" ];
  };
}
