{ config, lib, pkgs, ... }: let
  cfg = config.thoughtfull.restic;
  postgres-enabled = config.services.postgresql.enable;
  vaultwarden-enabled = config.services.vaultwarden.enable;
  webdav = config.services.webdav;
  enabled = postgres-enabled || vaultwarden-enabled || webdav.enable;
  pgbackup = config.services.postgresqlBackup;
in {
  options.thoughtfull.restic = {
    s3Bucket = lib.mkOption {
      type = lib.types.str;
      default = null;
      description = "Name of S3 bucket to store backups";
    };
    environmentFile = lib.mkOption {
      type = lib.types.str;
      default = null;
      description = lib.mdDoc ''
        file containing the credentials to access the repository, in the format of an
        EnvironmentFile as described by systemd.exec(5)
      '';
    };
    passwordFile = lib.mkOption {
      type = lib.types.str;
      default = null;
      description = lib.mdDoc ''
        Read the repository password from a file.
      '';
    };
  };
  config = lib.mkIf enabled {
    environment.systemPackages = [
      (pkgs.writeScriptBin "restic" ''
        #!/usr/bin/env bash
        ${pkgs.execline}/bin/envfile ${cfg.environmentFile} ${pkgs.restic}/bin/restic \
          -r "s3:s3.amazonaws.com/${cfg.s3Bucket}" \
          -p ${cfg.passwordFile} \
          "''${@}"
      '')
    ];
    services.restic.backups.default = (lib.mkMerge [
      {
        environmentFile = cfg.environmentFile;
        passwordFile = cfg.passwordFile;
        pruneOpts = [
          "--keep-daily 7"
          "--keep-weekly 5"
          "--keep-monthly 12"
          "--keep-yearly 75"
        ];
        repository = "s3:s3.amazonaws.com/${cfg.s3Bucket}";
        timerConfig.OnCalendar = lib.mkDefault "*-*-* *:00:00";
      }
      (lib.mkIf postgres-enabled {
        paths = [ pgbackup.location ];
      })
      (lib.mkIf vaultwarden-enabled {
        extraBackupArgs = [
          "--exclude=/var/lib/bitwarden_rs/icon_cache"
          "--exclude=/var/lib/bitwarden_rs/sends"
        ];
        paths = [ "/var/lib/bitwarden_rs" ];
      })
      (lib.mkIf webdav.enable {
        paths = [ webdav.settings.scope ];
      })
    ]);
  };
}
