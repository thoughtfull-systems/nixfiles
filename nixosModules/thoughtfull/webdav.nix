{ config, lib, ... }: let
  cfg = config.services.webdav;
in {
  services.webdav.settings = {
    address = lib.mkDefault "127.0.0.1";
    auth = lib.mkDefault true;
    cors = {
      enabled = lib.mkDefault false;
      credentials = lib.mkDefault false;
    };
    modify = lib.mkDefault true;
    port = lib.mkDefault 8001;
    scope = lib.mkDefault "/var/lib/webdav";
    tls = lib.mkDefault false;
    users = [{
      username = "webdav";
      password = "{env}WEBDAV_PASSWORD";
    }];
  };
  systemd.services.webdav.serviceConfig = lib.mkIf cfg.enable {
    StateDirectory = lib.mkDefault "webdav";
    StateDirectoryMode = lib.mkDefault "0700";
  };
  thoughtfull = {
    restic.paths = lib.mkIf cfg.enable [ cfg.settings.scope ];
    systemd-notify-failure.services = lib.mkIf cfg.enable [ "webdav" ];
  };
}
