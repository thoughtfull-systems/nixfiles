{ config, lib, pkgs, ... } : {
  options.thoughtfull.clojure = {
    enable = lib.mkEnableOption "clojure";
    jdk-package = lib.mkPackageOption pkgs "jdk" {
      default = "temurin-bin-17";
    };
  };
  config = lib.mkIf config.thoughtfull.clojure.enable {
    home.packages = with pkgs; [
      babashka
      clj-kondo
      (clojure.override {
        jdk = config.thoughtfull.clojure.jdk-package;
      })
      joker
    ] ++ [ config.thoughtfull.clojure.jdk-package ];
    programs.emacs = {
      extraConfig = "(require 'tfl-clojure)";
      extraPackages = epkgs: [ epkgs.tfl-clojure ];
    };
  };
}
