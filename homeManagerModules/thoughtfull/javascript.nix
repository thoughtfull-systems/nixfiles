{ config, lib, pkgs, ... } : let
  cfg = config.thoughtfull.javascript;
in {
  options.thoughtfull.javascript = {
    enable = lib.mkEnableOption "javascript";
    nodejs-package = lib.mkPackageOption pkgs "nodejs" {
      default = "nodejs_20";
    };
  };
  config = lib.mkIf cfg.enable {
    home.packages = [ cfg.nodejs-package ];
    programs.emacs = {
      extraConfig = "(require 'tfl-javascript)";
      extraPackages = epkgs: with epkgs; [
        tfl-javascript
      ];
    };
  };
}
