{ config, lib, pkgs, ... } : {
  options.thoughtfull.javascript = {
    enable = lib.mkEnableOption "javascript";
    nodejs-package = lib.mkPackageOption pkgs "nodejs" {
      default = "nodejs_20";
    };
  };
  config = {
    home.packages = [ config.thoughtfull.javascript.nodejs-package ];
    programs.emacs = {
      extraPackages = epkgs: with epkgs; [
        typescript-mode
      ];
    };
  };
}
