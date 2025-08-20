{ config, lib, osConfig, pkgs, ... }: let
  cfg = config.thoughtfull.mcp;
in {
  options.thoughtfull.mcp.enable = lib.mkOption {
    default = false;
    description = "Whether to enable mcp.";
    type = lib.types.bool;
  };
  config = lib.mkIf cfg.enable {
    programs.emacs = {
      enable = true;
      extraConfig = "(require 'tfl-mcp)";
      extraPackages = epkgs: [ epkgs.tfl-mcp ];
    };
  };
}
