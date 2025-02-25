{ config, lib, pkgs, ... } : let
  cfg = config.thoughtfull.tunnels;
in {
  options.thoughtfull.tunnels = lib.mkOption {
    type = lib.types.attrsOf (lib.types.submodule ({ name, ... }: {
      options = {
        name = lib.mkOption {
          type = lib.types.str;
          default = name;
          description = "name of the tunnel";
        };
        enable = lib.mkOption {
          default = true;
          description = "enable the tunnel";
          type = lib.types.bool;
        };
        user = lib.mkOption {
          default = "root";
          description = "user to connect with";
          type = lib.types.str;
        };
        host = lib.mkOption {
          description = "host to tunnel to";
          type = lib.types.str;
        };
        identity = lib.mkOption {
          description = "path to identity file to use for connection";
          type = lib.types.str;
        };
        bindings = lib.mkOption {
          description = "addresses and ports to bind";
          type = lib.types.listOf (lib.types.submodule {
            options = {
              local = {
                address = lib.mkOption {
                  default = "localhost";
                  description = "address to bind on the local end";
                  type = lib.types.str;
                };
                port = lib.mkOption {
                  description = "port to bind on the local end";
                  type = lib.types.int;
                };
              };
              remote = {
                address = lib.mkOption {
                  default = "localhost";
                  description = "address to bind on the remote end";
                  type = lib.types.str;
                };
                port = lib.mkOption {
                  description = "port to bind on the remote end";
                  type = lib.types.int;
                };
              };
            };
          });
        };
      };
    }));
  };
  config = lib.mkIf (builtins.length (builtins.attrNames cfg) > 0) {
    systemd.services = builtins.foldl'
      (services: name: let
        c = cfg.${name};
        endpoint = { address, port }: "${address}:${builtins.toString port}";
        bindings = (lib.strings.concatMapStringsSep " "
          (binding: "-R${endpoint binding.remote}:${endpoint binding.local}")
          c.bindings);
      in services // {
        "thoughtfull-tunnel-${name}" = {
          after = [ "network.target" ];
          enable = c.enable;
          script = ''
            ${pkgs.openssh}/bin/ssh -N -o ExitOnForwardFailure=yes \
                -i ${c.identity} \
                ${bindings} \
                ${c.user}@${c.host}
          '';
          serviceConfig = {
            Restart = lib.mkDefault "always";
            RestartMaxDelaySec = lib.mkDefault 300;
            RestartSec = lib.mkDefault 5;
            RestartSteps = lib.mkDefault 100;
            Type = lib.mkDefault "exec";
          };
          wantedBy = [ "multi-user.target" ];
        };
      })
      {}
      (builtins.attrNames cfg);
    thoughtfull.systemd-notify-failure.services = (builtins.map
      (name: "thoughtfull-tunnel-${name}")
      (builtins.attrNames cfg));
  };
}
