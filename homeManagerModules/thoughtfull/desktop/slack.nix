{ config, lib, pkgs, ... }: let
  cfg = config.thoughtfull.desktop.slack;
in {
  options.thoughtfull.desktop.slack.enable = lib.mkOption {
    default = config.thoughtfull.desktop.enable;
    example = false;
    description = "Whether to enable slack.";
    type = lib.types.bool;
  };
  config = lib.mkIf cfg.enable {
    home.packages = [ pkgs.unstable.slack ];
  };
}
