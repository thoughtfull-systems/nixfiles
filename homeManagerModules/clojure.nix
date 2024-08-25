{ config, lib, pkgs, ... } : let
  cfg = config.thoughtfull.clojure;
in {
  options.thoughtfull.clojure = {
    enable = lib.mkEnableOption "clojure";
    jdk-package = lib.mkPackageOption pkgs "jdk" {
      default = "temurin-bin-17";
    };
  };
  config = lib.mkIf cfg.enable {
    home.packages = with pkgs; [
      babashka
      clj-kondo
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
