{ config, lib, pkgs, ... } : let
  cfg = config.thoughtfull.tunnel;
  portStrs = builtins.map builtins.toString cfg.ports;
  portToArg = portStr: " -R${portStr}:localhost:${portStr}";
  portsString = lib.strings.concatMapStrings portToArg portStrs;
in {
  options.thoughtfull.tunnel = {
    enable = lib.mkEnableOption "tunnel";
    user = lib.mkOption {
      default = "root";
      description = "user to use connecting to host";
      type = lib.types.str;
    };
    host = lib.mkOption {
      description = "host to tunnel to";
      type = lib.types.str;
    };
    identity = lib.mkOption {
      description = "path to identity file to use for tunnel";
      type = lib.types.str;
    };
    ports = lib.mkOption {
      description = "ports to tunnel";
      type = lib.types.listOf lib.types.int;
    };
  };
  config = lib.mkIf (cfg.enable && (builtins.length cfg.ports) > 0) {
    systemd.services.thoughtfull-tunnel = {
      after = [ "network.target" ];
      enable = true;
      script = ''
        ${pkgs.openssh}/bin/ssh -N -o ExitOnForwardFailure=yes -i ${cfg.identity} ${cfg.user}@${cfg.host} ${portsString}
      '';
      serviceConfig = {
        Restart = "always";
        Type = "exec";
      };
      wantedBy = [ "multi-user.target" ];
    };
  };
}
