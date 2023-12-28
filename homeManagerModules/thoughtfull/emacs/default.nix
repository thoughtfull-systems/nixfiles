{ config, lib, pkgs, ... }: lib.mkIf config.programs.emacs.enable {
  home = {
    packages = (lib.mkMerge [
      (with pkgs; [
        aspell
        aspellDicts.en
        aspellDicts.en-computers
        aspellDicts.en-science
        silver-searcher
        # for loading files from JARs
        unzip
      ])
      (lib.mkIf config.thoughtfull.desktop.enable (with pkgs; [
        emacs-all-the-icons-fonts
        source-code-pro
      ]))
    ]);
  };
  programs.emacs = {
    extraConfig = lib.mkBefore ''
      (require 'use-package)
      (require 'my)
    '';
    extraPackages = epkgs: with epkgs; [
      my
      use-package
    ];
    package = pkgs.emacs29;
  };
}
