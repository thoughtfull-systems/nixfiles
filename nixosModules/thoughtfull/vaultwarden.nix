{ config, lib, ... }: let
  vaultwarden = config.services.vaultwarden.enable;
in lib.mkIf vaultwarden {
  services = {
    postgresql = {
      enable = true;
      ensureDatabases = [ "vaultwarden" ];
      ensureUsers = [{
        name = "vaultwarden";
        ensureDBOwnership = true;
      }];
    };
    postgresqlBackup.databases = [ "vaultwarden" ];
    vaultwarden = {
      config = {
        DATABASE_URL = "postgresql:///vaultwarden";
        ROCKET_PORT = 8000;
      };
      dbBackend = "postgresql";
    };
  };
  thoughtfull = {
    restic = {
      exclude = [
        "/var/lib/bitwarden_rs/icon_cache"
        "/var/lib/bitwarden_rs/sends"
      ];
      paths = [ "/var/lib/bitwarden_rs" ];
    };
    systemd-notify-failure.services = [
      "postgresqlBackup-vaultwarden"
      "vaultwarden"
    ];
  };
}
