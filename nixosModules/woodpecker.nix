{ config, lib, ... }: let
  enabled = config.services.woodpecker-server.enable;
  cfg = config.thoughtfull.woodpecker;
in {
  options.thoughtfull.woodpecker.age = {
    agent.environmentFile = lib.mkOption {
      description = "age encrypted file containing environment for woodpecker agent";
      type = lib.types.path;
    };
    server.environmentFile = lib.mkOption {
      description = "age encrypted file containing environment for woodpecker server";
      type = lib.types.path;
    };
  };
  config = lib.mkIf enabled {
    age.secrets = {
      woodpecker-agent-environment.file = cfg.age.agent.environmentFile;
      woodpecker-server-environment.file = cfg.age.server.environmentFile;
    };
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
      woodpecker-agents.agents.podman.environmentFile = [
        config.age.secrets.woodpecker-agent-environment.path
      ];
      woodpecker-server = {
        environment = {
          WOODPECKER_SERVER_ADDR = lib.mkDefault ":8004";
          WOODPECKER_DATABASE_DRIVER = lib.mkDefault "postgres";
          WOODPECKER_DATABASE_DATASOURCE = lib.mkDefault "postgres:///woodpecker?host=/run/postgresql";
        };
        environmentFile = config.age.secrets.woodpecker-server-environment.path;
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
  };
}
