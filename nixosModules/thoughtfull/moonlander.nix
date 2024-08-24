{ config, lib, pkgs, ... }: let
  cfg = config.thoughtfull.moonlander;
in {
  options.thoughtfull.moonlander.enable = lib.mkEnableOption "moonlander";
  config = lib.mkIf cfg.enable {
    environment.systemPackages = [ pkgs.wally-cli ];
  };
}
