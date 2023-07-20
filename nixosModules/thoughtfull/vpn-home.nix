{ config, lib, ... }: let
  cfg = config.thoughtfull.vpn.home;
  secrets = config.age.secrets;
in {
  options.thoughtfull.vpn.home = {
    enable = lib.mkEnableOption "Home VPN";
    autoStart = lib.mkOption {
      default = false;
      type = lib.types.bool;
      description = lib.mdDoc "Whether Home VPN should be started automatically.";
    };
  };
  config = lib.mkIf cfg.enable {
    age.secrets = {
      vpn-home-askpass.file = ../../age/secrets/vpn-home-askpass.age;
      vpn-home-config.file = ../../age/secrets/vpn-home-config.age;
    };
    services = lib.mkDefault {
      openvpn.servers.home = {
        autoStart = lib.mkDefault cfg.autoStart;
        config = ''
          config ${secrets.vpn-home-config.path}
          askpass ${secrets.vpn-home-askpass.path}
        '';
        updateResolvConf = true;
      };
    };
  };
}
