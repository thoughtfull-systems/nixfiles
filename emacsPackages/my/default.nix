epkgs: {
  my = epkgs.trivialBuild {
    packageRequires = with epkgs; [
      all-the-icons
      all-the-icons-completion
      all-the-icons-dired
      all-the-icons-ibuffer
      company
      flycheck
      magit
      marginalia
      nix-mode
      orderless
      paredit
      wgrep
      writegood-mode
    ];
    pname = "my";
    src = ./src;
    version = "0";
  };
}
