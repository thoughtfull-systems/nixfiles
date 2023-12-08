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
    extraPackages = epkgs: with epkgs; [
      my
      my-completion
      my-prog
    ];
    package = pkgs.emacs29;
  };
}
