{ config, lib, pkgs, ... }: let
  cfg = config.thoughtfull.services.xbanish;
in {
  options.thoughtfull.services.xbanish.enable = lib.mkOption {
    default = config.thoughtfull.desktop.enable;
    example = false;
    description = "Whether to enable xbanish.";
    type = lib.types.bool;
  };
  config = lib.mkIf cfg.enable {
    home.packages = [ pkgs.xbanish ];
    systemd.user.services.xbanish = {
      Install.WantedBy = [ "hm-graphical-session.target" ];
      Service = {
        ExecStart = "${pkgs.xbanish}/bin/xbanish -i control -i mod4";
        Restart = "always";
      };
      Unit = {
        After = [ "hm-graphical-session.target" ];
        Description = "xbanish hides the mouse pointer while typing";
        Requires = [ "hm-graphical-session.target" ];
      };
    };
  };
}
