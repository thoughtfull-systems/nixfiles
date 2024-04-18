{ config, lib, pkgs, ... }: lib.mkIf config.programs.emacs.enable {
  home = {
    file.".config/emacs/early-init.el".source = ./emacs/early-init.el;
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
      (require 'tfl)
    '';
    extraPackages = epkgs: [ epkgs.tfl ];
    # moving emacs29->emacs29-gtk3 fixes a focusing issue with EXWM--switching from X buffer to
    # another X buffer, but it does not fix switching from an emacs buffer to an X buffer.  it's a
    # long running and complicated issue.  a good reference is
    # https://github.com/ch11ng/exwm/issues/759. the best theory seems to be libX11 version. I guess
    # this change uses a different version (either older or newer)?
    package = pkgs.emacs29-gtk3;
  };
}
