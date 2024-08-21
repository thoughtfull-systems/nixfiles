{ config, lib, ... }: {
  config = lib.mkIf config.services.postgresql.enable {
    services.postgresqlBackup = {
      enable = true;
      startAt = lib.mkDefault "*-*-* *:55:00";
    };
    thoughtfull.systemd-notify-failure.services = [ "postgresqlBackup" ];
  };
}
