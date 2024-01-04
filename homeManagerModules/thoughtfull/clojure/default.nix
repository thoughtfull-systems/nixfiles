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
      extraConfig = builtins.readFile ./extra-config.el;
      extraPackages = epkgs: with epkgs; [
        cider
        clojure-mode
        clojure-mode-extra-font-locking
        flycheck
        flycheck-clj-kondo
      ];
    };
  };
}
