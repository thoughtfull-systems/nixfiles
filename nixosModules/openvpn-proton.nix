{ config, lib, ... }: let
  cfg = config.thoughtfull.openvpn.proton;
  secrets = config.age.secrets;
in {
  options.thoughtfull.openvpn.proton = {
    enable = lib.mkEnableOption "Proton VPN";
    age = {
      authFile = lib.mkOption {
        default = true;
        type = lib.types.path;
        description = lib.mdDoc "Age encrypted file containing Proton VPN authentication";
      };
      configFile = lib.mkOption {
        default = true;
        type = lib.types.path;
        description = lib.mdDoc "Age encrypted file containing Proton VPN configuration";
      };
    };
    autoStart = lib.mkOption {
      default = true;
      type = lib.types.bool;
      description = lib.mdDoc "Whether Proton VPN should be started automatically.";
    };
  };
  config = lib.mkIf cfg.enable {
    age.secrets = {
      thoughtfull-vpn-proton-auth.file = cfg.age.authFile;
      thoughtfull-vpn-proton-config.file = cfg.age.configFile;
    };
    services = lib.mkDefault {
      openvpn.servers.proton = {
        autoStart = cfg.autoStart;
        config = ''
          config ${secrets.thoughtfull-vpn-proton-config.path}
          auth-user-pass ${secrets.thoughtfull-vpn-proton-auth.path}
        '';
        updateResolvConf = true;
      };
    };
  };
}
