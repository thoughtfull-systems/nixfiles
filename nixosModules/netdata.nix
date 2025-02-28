{ config, lib, pkgs, ... } : let
  enabled = config.services.netdata.enable;
  cfg = config.thoughtfull.netdata;
in {
  options.thoughtfull.netdata = {
    api-key = lib.mkOption {
      default = null;
      description = "netdata api key";
      type = lib.types.str;
    };
    mode = lib.mkOption {
      default = "child";
      description = "mode of operation";
      type = lib.types.enum [ "child" "parent" ];
    };
    parent.host = lib.mkOption {
      description = "parent netdata host";
      type = lib.types.str;
    };
  };
  config = {
    services.netdata = lib.mkIf enabled
      (if cfg.mode == "parent" then {
        config = {
          db = {
            "mode" = "dbengine";
          };
          ml = {
            "enabled" = "no";
          };
          web = {
            "web server threads" = "1";
          };
        };
        configDir."stream.conf" = pkgs.writeText "stream.conf" ''
          [${cfg.api-key}]
          enabled = yes
        '';
      } else {
        config = {
          db = {
            "mode" = "alloc";
          };
          health = {
            "enabled" = "no";
          };
          web = {
            "mode" = "none";
          };
        };
        configDir."stream.conf" = pkgs.writeText "stream.conf" ''
          [stream]
          enabled = yes
          destination = ${cfg.parent.host}
          api key = ${cfg.api-key}
        '';
      });
  };
}
