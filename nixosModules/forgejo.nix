{ config, lib, pkgs, ... }: let
  cfg = config.services.forgejo;
in {
  environment.systemPackages = lib.mkIf cfg.enable [ pkgs.forgejo ];
  services.forgejo = {
    database.type = lib.mkDefault "postgres";
    dump.enable = lib.mkDefault true;
    package = pkgs.forgejo;
    settings = {
      service.DISABLE_REGISTRATION = lib.mkDefault true;
      server.HTTP_PORT = lib.mkDefault 8003;
    };
  };
  systemd.services.forgejo = lib.mkIf cfg.enable {
    serviceConfig = {
      RestartMaxDelaySec = lib.mkDefault 300;
      RestartSec = lib.mkDefault 5;
      RestartSteps = lib.mkDefault 100;
    };
  };
  thoughtfull = lib.mkIf cfg.enable {
    restic.paths = [ cfg.dump.backupDir ];
    systemd-notify-failure.services = [ "forgejo" ];
  };
}
