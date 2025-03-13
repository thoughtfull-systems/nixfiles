{ config, lib, ... }: let
  cfg = config.thoughtfull.gotosocial;
  gotosocial = config.services.gotosocial.enable;
in {
  options.thoughtfull.gotosocial.age = {
    environmentFile = lib.mkOption {
      description = "Encrypted age file containing gotosocial environment file";
      type = lib.types.path;
    };
  };
  config = lib.mkIf gotosocial {
    age.secrets.thoughtfull-gotosocial-environment = {
      file = cfg.age.environmentFile;
      owner = "gotosocial";
    };
    services = {
      gotosocial = {
        environmentFile = config.age.secrets.thoughtfull-gotosocial-environment.path;
        settings = {
          account-domain = lib.mkOverride 900 "thoughtfull.systems";
          application-name = lib.mkOverride 900 "Thoughtfull Systems";
          bind-address = lib.mkOverride 900 "localhost";
          cache.memory-target = lib.mkOverride 900 "50MiB";
          db-max-open-conns-multiplier = lib.mkOverride 900 1;
          host = lib.mkOverride 900 "social.thoughtfull.systems";
          instance-languages = ["en"];
          landing-page-user = lib.mkOverride 900 "technosophist";
          letsencrypt-enabled = lib.mkOverride 900 false;
          port = lib.mkOverride 900 8002;
          protocol = lib.mkOverride 900 "https";
        };
        setupPostgresqlDB = lib.mkOverride 900 true;
      };
      postgresqlBackup.databases = [ "gotosocial" ];
    };
    thoughtfull = {
      restic.paths = [ "/var/lib/gotosocial" ];
      systemd-notify-failure.services = [ "gotosocial" ];
    };
  };
}
