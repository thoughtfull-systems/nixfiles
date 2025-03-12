{ config, lib, pkgs, ... } : let
  cfg = config.thoughtfull.clojure;
in {
  options.thoughtfull.clojure = {
    enable = lib.mkEnableOption "clojure";
    babashka-package = lib.mkPackageOption pkgs "babashka" {
      default = "babashka";
    };
    clj-kondo-package = lib.mkPackageOption pkgs "clj-kondo" {
      default = "clj-kondo";
    };
    jdk-package = lib.mkPackageOption pkgs "jdk" {
      default = "temurin-bin-17";
    };
  };
  config = lib.mkIf cfg.enable {
    home.packages = with pkgs; [
      cfg.babashka-package
      cfg.clj-kondo-package
      (clojure.override {
        jdk = cfg.jdk-package;
      })
      joker
    ] ++ [ cfg.jdk-package ];
    programs.emacs = {
      extraConfig = "(require 'tfl-clojure)";
      extraPackages = epkgs: [ epkgs.tfl-clojure ];
    };
  };
}
