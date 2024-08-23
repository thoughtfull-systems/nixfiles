{ config, lib, pkgs, ... }: let
  moonlander = config.thoughtfull.moonlander.enable;
in {
  options.thoughtfull.moonlander.enable = lib.mkEnableOption "moonlander";
  config = lib.mkIf moonlander {
    environment.systemPackages = [ pkgs.wally-cli ];
  };
}
