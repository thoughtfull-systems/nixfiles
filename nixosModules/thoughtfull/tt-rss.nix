{ config, lib, pkgs, ... }: let
  cfg = config.services.tt-rss;
in lib.mkIf cfg.enable {
  # needed for database migrations
  environment.systemPackages = [ pkgs.php ];
  services = {
    postgresqlBackup.databases = [ cfg.database.name ];
    tt-rss = {
      database = {
        createLocally = true;
        type = "pgsql";
      };
      registration.enable = true;
    };
  };
  thoughtfull.systemd-notify-failure.services = [ "tt-rss" ];
}
