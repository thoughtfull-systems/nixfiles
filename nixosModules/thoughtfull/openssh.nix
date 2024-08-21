{ config, lib, ... } : let
  cfg = config.services.openssh;
in {
  config = {
    services.openssh = lib.mkMerge [
      {
        enable = lib.mkOverride 900 (!config.thoughtfull.desktop.enable);
      }
      (lib.mkIf cfg.enable {
        settings.PasswordAuthentication = lib.mkDefault false;
      })
    ];
    thoughtfull.systemd-notify-failure.services = [ "sshd" ];
  };
}
