epkgs: {
  tfl-rust = epkgs.trivialBuild {
    packageRequires = with epkgs; [
      flycheck-rust
      rust-mode
      tfl
    ];
    pname = "tfl-rust";
    src = ./tfl-rust.el;
    version = "0";
  };
}
