{ config, lib, ... }: let
  cfg = config.thoughtfull.webdav;
  webdav = config.services.webdav;
in {
  options.thoughtfull.webdav.age = {
    environmentFile = lib.mkOption {
      type = lib.types.path;
      default = null;
      description = ''
        Age encrypted environment file as defined in {manpage}`systemd.exec(5)`
      '';
    };
  };
  config = {
    age.secrets = lib.mkIf webdav.enable {
      thoughtfull-webdav-env-file.file = cfg.age.environmentFile;
    };
    services.webdav = {
      environmentFile = config.age.secrets.thoughtfull-webdav-env-file.path;
      settings = {
        address = lib.mkDefault "127.0.0.1";
        auth = lib.mkDefault true;
        behindProxy = lib.mkDefault true;
        cors = {
          enabled = lib.mkDefault false;
          credentials = lib.mkDefault false;
        };
        directory = lib.mkDefault "/var/lib/webdav";
        modify = lib.mkDefault true;
        permissions = "CRUD";
        port = lib.mkDefault 8001;
        tls = lib.mkDefault false;
        users = [{
          username = "webdav";
          password = "{env}WEBDAV_PASSWORD";
        }];
      };
    };
    systemd.services.webdav.serviceConfig = lib.mkIf webdav.enable {
      StateDirectory = lib.mkDefault "webdav";
      StateDirectoryMode = lib.mkDefault "0700";
    };
    thoughtfull = {
      restic.paths = lib.mkIf webdav.enable [ webdav.settings.scope ];
      systemd-notify-failure.services = lib.mkIf webdav.enable [ "webdav" ];
    };
  };
}
