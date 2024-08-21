{ config, lib, pkgs, ... }: lib.mkIf config.services.tt-rss.enable {
  # needed for database migrations
  environment.systemPackages = [ pkgs.php ];
  networking.firewall.allowedTCPPorts = [ 80 ];
  services = {
    postgresql = {
      enable = true;
      ensureDatabases = [ "tt_rss" ];
      ensureUsers = [{
        ensureDBOwnership = true;
        name = "tt_rss";
      }];
    };
    postgresqlBackup.databases = [ "tt_rss" ];
    tt-rss = {
      database.createLocally = false;
      registration.enable = true;
    };
  };
  thoughtfull.systemd-notify-failure.services = [ "tt-rss" ];
}
