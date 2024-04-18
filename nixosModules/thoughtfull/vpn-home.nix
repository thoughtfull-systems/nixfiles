{ config, lib, secrets, ... }: let
  cfg = config.thoughtfull.vpn.home;
  age = config.age.secrets;
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
      vpn-home-askpass.file = secrets.age.vpn-home-askpass;
      vpn-home-config.file = secrets.age.vpn-home-config;
    };
    services = lib.mkDefault {
      openvpn.servers.home = {
        autoStart = lib.mkDefault cfg.autoStart;
        config = ''
          config ${age.vpn-home-config.path}
          askpass ${age.vpn-home-askpass.path}
        '';
        updateResolvConf = true;
      };
    };
  };
}
