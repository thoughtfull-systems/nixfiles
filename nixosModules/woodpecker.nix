{ config, lib, ... }: let
  woodpecker = config.services.woodpecker-server.enable;
in lib.mkIf woodpecker {
  services = {
    postgresql = {
      ensureDatabases = [ "woodpecker" ];
      ensureUsers = [
        {
          name = "woodpecker";
          ensureDBOwnership = true;
        }
      ];
    };
    woodpecker-server = {
      # enable = true;
      environment = {
        WOODPECKER_SERVER_ADDR = lib.mkDefault ":8004";
        WOODPECKER_DATABASE_DRIVER = lib.mkDefault "postgres";
        WOODPECKER_DATABASE_DATASOURCE = "postgres:///woodpecker?host=/run/postgresql";
      };
    };
    postgresqlBackup.databases = [ "woodpecker" ];
  };
  systemd.services.woodpecker-server.serviceConfig = {
    DynamicUser = lib.mkForce false;
    User = "woodpecker";
  };
  thoughtfull = {
    systemd-notify-failure.services = [ "woodpecker-server" ];
  };
  users = {
    groups.woodpecker = {};
    users.woodpecker = {
      group = "woodpecker";
      isSystemUser = lib.mkDefault true;
      uid = lib.mkDefault 8004;
    };
  };
}
