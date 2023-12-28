epkgs: {
  my = epkgs.trivialBuild {
    packageRequires = with epkgs; [
      all-the-icons
      all-the-icons-completion
      all-the-icons-dired
      all-the-icons-ibuffer
      marginalia
      nix-mode
      orderless
      wgrep
      writegood-mode
    ];
    pname = "my";
    src = ./src;
    version = "0";
  };
}
