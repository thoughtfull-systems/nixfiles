{ config, lib, ... }: let
  cfg = config.thoughtfull.vpn.proton;
  secrets = config.age.secrets;
in {
  options.thoughtfull.vpn.proton.enable = lib.mkEnableOption "proton vpn";
  config = lib.mkIf cfg.enable {
    age.secrets = {
      vpn-proton-config.file = ../../age/secrets/vpn-proton-config.age;
      vpn-proton-auth-user-pass.file = ../../age/secrets/vpn-proton-auth-user-pass.age;
    };
    services = lib.mkDefault {
      openvpn.servers.proton = {
        config = ''
          config ${secrets.vpn-proton-config.path}
          auth-user-pass ${secrets.vpn-proton-auth-user-pass.path}
        '';
        updateResolvConf = true;
      };
    };
  };
}
