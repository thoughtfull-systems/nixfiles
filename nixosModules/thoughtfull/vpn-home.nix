{ config, lib, ... }: let
  cfg = config.thoughtfull.vpn.home;
  secrets = config.age.secrets;
in {
  options.thoughtfull.vpn.home.enable = lib.mkEnableOption "proton vpn";
  config = lib.mkIf cfg.enable {
    age.secrets = {
      vpn-home-config.file = ../../age/secrets/vpn-home-config.age;
      vpn-home-askpass.file = ../../age/secrets/vpn-home-askpass.age;
    };
    services = lib.mkDefault {
      openvpn.servers.home = {
        autoStart = false;
        config = ''
          config ${secrets.vpn-home-config.path}
          askpass ${secrets.vpn-home-askpass.path}
        '';
        updateResolvConf = true;
      };
    };
  };
}
