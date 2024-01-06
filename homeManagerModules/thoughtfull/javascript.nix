{ config, lib, pkgs, ... } : {
  options.thoughtfull.javascript = {
    enable = lib.mkEnableOption "javascript";
    nodejs-package = lib.mkPackageOption pkgs "nodejs" {
      default = "nodejs_20";
    };
  };
  config = lib.mkIf config.thoughtfull.javascript.enable {
    home.packages = [ config.thoughtfull.javascript.nodejs-package ];
    programs.emacs = {
      extraConfig = "(require 'tfl-javascript)";
      extraPackages = epkgs: with epkgs; [
        tfl-javascript
      ];
    };
  };
}
