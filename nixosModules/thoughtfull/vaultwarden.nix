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
  thoughtfull.systemd-notify-failure.services = [ "vaultwarden" ];
}
