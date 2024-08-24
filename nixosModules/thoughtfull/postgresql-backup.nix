{ config, lib, ... }: let
  cfg = config.services.postgresqlBackup;
in {
  config = {
    services.postgresqlBackup = {
      enable = lib.mkDefault config.services.postgresql.enable;
      startAt = lib.mkDefault "*-*-* *:55:00";
    };
    thoughtfull.restic.paths = lib.mkIf cfg.enable [ cfg.location ];
  };
}
