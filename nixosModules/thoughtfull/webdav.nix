{ config, lib, ... }: let
  webdav = config.services.webdav.enable;
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
  systemd.services.webdav.serviceConfig = lib.mkIf webdav {
    StateDirectory = lib.mkDefault "webdav";
    StateDirectoryMode = lib.mkDefault "0700";
  };
  thoughtfull.systemd-notify-failure.services = lib.mkIf webdav [ "webdav" ];
}
