{ lib, pkgs, ... } : {
  options.thoughtfull.clojure.enable = lib.mkEnableOption "clojure";
  config = {
    home.packages = with pkgs; [
      adoptopenjdk-hotspot-bin-11
      babashka
      clj-kondo
      clojure
      joker
    ];
    programs.emacs = {
      extraPackages = epkgs: with epkgs; [
        my-clojure-dev
      ];
    };
  };
}
