{ config, lib, ... }: let
  cfg = config.services.postgresqlBackup;
in {
  config = {
    services.postgresqlBackup = {
      enable = lib.mkDefault config.services.postgresql.enable;
      startAt = lib.mkDefault "*-*-* *:55:00";
    };
    thoughtfull = {
      restic.paths = lib.mkIf cfg.enable [ cfg.location ];
      systemd-notify-failure.services = lib.mkIf cfg.enable
        (if cfg.backupAll then
          [ "postgresqlBackup" ]
         else
           (builtins.map
             (db: "postgresqlBackup-${db}")
             cfg.databases));
    };
  };
}
