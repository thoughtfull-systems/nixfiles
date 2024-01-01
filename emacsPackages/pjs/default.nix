epkgs: {
  pjs = epkgs.trivialBuild {
    packageRequires = with epkgs; [
      all-the-icons
      all-the-icons-completion
      all-the-icons-dired
      all-the-icons-ibuffer
      company
      consult
      diminish
      flycheck
      magit
      marginalia
      nix-mode
      orderless
      paredit
      use-package
      wgrep
      writegood-mode
    ];
    pname = "pjs";
    src = ./src;
    version = "0";
  };
}
