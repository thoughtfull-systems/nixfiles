{ config, lib, pkgs, ... }: let
  cfg = config.thoughtfull.desktop.discord;
in {
  options.thoughtfull.desktop.discord.enable = lib.mkOption {
    default = config.thoughtfull.desktop.enable;
    example = false;
    description = "Whether to enable discord.";
    type = lib.types.bool;
  };
  config = lib.mkIf cfg.enable {
    home.packages = [ pkgs.discord ];
  };
}
