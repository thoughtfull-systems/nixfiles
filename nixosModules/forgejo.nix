{ config, lib, ... }: let
  cfg = config.services.forgejo;
in {
  services.forgejo = {
    database.type = lib.mkDefault "postgres";
    dump.enable = lib.mkDefault true;
    settings = {
      service.DISABLE_REGISTRATION = lib.mkDefault true;
      server.HTTP_PORT = lib.mkDefault 8003;
    };
  };
  thoughtfull = lib.mkIf cfg.enable {
    restic.paths = [ cfg.dump.backupDir ];
    systemd-notify-failure.services = [ "forgejo" ];
  };
}
