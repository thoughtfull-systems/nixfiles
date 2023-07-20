{ config, lib, ... }: let
  cfg = config.thoughtfull.vpn.proton;
  secrets = config.age.secrets;
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
      vpn-proton-auth-user-pass.file = ../../age/secrets/vpn-proton-auth-user-pass.age;
      vpn-proton-config.file = ../../age/secrets/vpn-proton-config.age;
    };
    services = lib.mkDefault {
      openvpn.servers.proton = {
        autoStart = lib.mkDefault cfg.autoStart;
        config = ''
          config ${secrets.vpn-proton-config.path}
          auth-user-pass ${secrets.vpn-proton-auth-user-pass.path}
        '';
        updateResolvConf = true;
      };
    };
  };
}
