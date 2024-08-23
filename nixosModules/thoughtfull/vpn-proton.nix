{ config, lib, secrets, ... }: let
  cfg = config.thoughtfull.vpn.proton;
  age = config.age.secrets;
in {
  options.thoughtfull.vpn.proton = {
    enable = lib.mkEnableOption "Proton VPN";
    autoStart = lib.mkOption {
      default = true;
      type = lib.types.bool;
      description = lib.mdDoc "Whether Proton VPN should be started automatically.";
    };
  };
  config = lib.mkIf cfg.enable {
    age.secrets = {
      vpn-proton-auth-user-pass.file = secrets.age.vpn-proton-auth-user-pass;
      vpn-proton-config.file = secrets.age.vpn-proton-config;
    };
    services = lib.mkDefault {
      openvpn.servers.proton = {
        autoStart = cfg.autoStart;
        config = ''
          config ${age.vpn-proton-config.path}
          auth-user-pass ${age.vpn-proton-auth-user-pass.path}
        '';
        updateResolvConf = true;
      };
    };
  };
}
