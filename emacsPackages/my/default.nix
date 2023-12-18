epkgs: {
  my = epkgs.elpaBuild {
    packageRequires = with epkgs; [
      all-the-icons
      all-the-icons-completion
      all-the-icons-dired
      all-the-icons-ibuffer
      nix-mode
      use-package
      wgrep
    ];
    pname = "my";
    src = ./my.el;
    version = "0.0.0";
  };
}
