{ config, lib, pkgs, ... }: let
  cfg = config.thoughtfull.deploy-keys;
in {
  options.thoughtfull.deploy-keys = lib.mkOption {
    default = [];
    description = lib.mdDoc ''
    '';
    type = lib.types.listOf (
      lib.types.submodule (
        { name, ... }: {
          options = {
            name = lib.mkOption {
              default = name;
              description = lib.mdDoc ''
                Name of the deploy key.  Prepended to hostname for ssh config.
              '';
              type = lib.types.str;
            };
            hostname = lib.mkOption {
              default = "github.com";
              description = lib.mdDoc ''
                Hostname combined with name in ssh config.
              '';
              type = lib.types.str;
            };
          };
        }
      )
    );
  };
  config = lib.mkIf (cfg != []) {
    programs.ssh.extraConfig = (lib.concatMapStringsSep "\n"
      ({ name, hostname }: ''
        Host ${name}.${hostname}
        Hostname ${hostname}
        IdentityFile /etc/nixos/${name}-deploy-key
      '')
      cfg);
    system.activationScripts = {
      ensure-deploy-keys = lib.concatMapStringsSep "\n"
        ({ name, ... }: ''
          [ -f /etc/nixos/${name}-deploy-key ] || \
            ${pkgs.openssh}/bin/ssh-keygen -f /etc/nixos/${name}-deploy-key -t ed25519 -N "" \
              -C "${name} deploy key"
        '')
        cfg;
    };
  };
}
