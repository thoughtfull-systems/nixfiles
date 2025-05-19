epkgs: {
  tfl = epkgs.trivialBuild {
    packageRequires = with epkgs; [
      all-the-icons
      all-the-icons-completion
      all-the-icons-dired
      all-the-icons-ibuffer
      # Install manually until 25.05 is released
      #
      # chatgpt-shell
      company
      consult
      dap-mode
      diminish
      flycheck
      lsp-mode
      lsp-ui
      magit
      marginalia
      markdown-mode
      nix-mode
      orderless
      paredit
      use-package
      vertico
      visual-fill-column
      wgrep
      which-key
      writegood-mode
      yaml-mode
    ];
    pname = "tfl";
    src = ./src;
    version = "0";
  };
}
