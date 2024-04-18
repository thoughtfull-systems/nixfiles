{ config, lib, ... }: lib.mkIf config.services.vaultwarden.enable {
  networking.firewall.allowedTCPPorts = [ 8000 ];
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
}
