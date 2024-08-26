{ config, lib, pkgs, ... }: let
  cfg = config.thoughtfull.restic;
in {
  options.thoughtfull.restic = {
    age = {
      environmentFile = lib.mkOption {
        type = lib.types.path;
        default = null;
        description = lib.mdDoc ''
          Age encrypted file containing the credentials to access the repository, in the format of
          an EnvironmentFile as described by systemd.exec(5)
        '';
      };
      passwordFile = lib.mkOption {
        type = lib.types.path;
        default = null;
        description = lib.mdDoc ''
          Age encrypted file containing the repository password
        '';
      };
    };
    s3Bucket = lib.mkOption {
      type = lib.types.str;
      default = null;
      description = "Name of S3 bucket to store backups";
    };
    exclude = lib.mkOption {
      type = lib.types.listOf lib.types.str;
      default = [];
      description = ''
        Patterns to exclude when backing up. See
        https://restic.readthedocs.io/en/latest/040_backup.html#excluding-files for
        details on syntax.
      '';
      example = [
        "/var/cache"
        "/home/*/.cache"
        ".git"
      ];
    };
    paths = lib.mkOption {
      type = lib.types.listOf lib.types.str;
      default = [ ];
      description = ''
        Which paths to backup, in addition to ones specified via
        `dynamicFilesFrom`.  If null or an empty array and
        `dynamicFilesFrom` is also null, no backup command will be run.
         This can be used to create a prune-only job.
      '';
      example = [
        "/var/lib/postgresql"
        "/home/user/backup"
      ];
    };
  };
  config = lib.mkIf (cfg.paths != []) (let
    env-path = config.age.secrets.thoughtfull-restic-env-file.path;
    pwd-path = config.age.secrets.thoughtfull-restic-pwd-file.path;
  in {
    age.secrets = {
      thoughtfull-restic-env-file.file = cfg.age.environmentFile;
      thoughtfull-restic-pwd-file.file = cfg.age.passwordFile;
    };
    environment.systemPackages = [
      (pkgs.writeScriptBin "restic" ''
        #!${pkgs.bash}/bin/bash
        ${pkgs.execline}/bin/envfile ${env-path} ${pkgs.restic}/bin/restic \
          -r "s3:s3.amazonaws.com/${cfg.s3Bucket}" \
          -p ${pwd-path} \
          "''${@}"
      '')
    ];
    services.restic.backups.default = {
      environmentFile = env-path;
      exclude = cfg.exclude;
      passwordFile = pwd-path;
      paths = cfg.paths;
      pruneOpts = [
        "--keep-daily 7"
        "--keep-weekly 5"
        "--keep-monthly 12"
        "--keep-yearly 75"
      ];
      repository = "s3:s3.amazonaws.com/${cfg.s3Bucket}";
      timerConfig.OnCalendar = lib.mkDefault "*-*-* *:00:00";
    };
    thoughtfull.systemd-notify-failure.services = [ "restic-backups-default" ];
  });
}
