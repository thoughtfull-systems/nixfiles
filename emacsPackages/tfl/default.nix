epkgs: {
  tfl = epkgs.trivialBuild {
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
      vertico
      visual-fill-column
      wgrep
      which-key
      writegood-mode
    ];
    pname = "tfl";
    src = ./src;
    version = "0";
  };
}
