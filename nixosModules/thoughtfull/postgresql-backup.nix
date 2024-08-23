{ config, lib, ... }: let
  postgresql = config.services.postgresql.enable;
  postgresqlBackup = config.services.postgresqlBackup.enable;
in {
  config = {
    services.postgresqlBackup = {
      enable = lib.mkDefault postgresql;
      startAt = lib.mkDefault "*-*-* *:55:00";
    };
    thoughtfull.systemd-notify-failure.services = lib.mkIf postgresqlBackup [ "postgresqlBackup" ];
  };
}
