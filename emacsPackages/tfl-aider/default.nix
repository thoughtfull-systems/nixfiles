epkgs: {
  tfl-aider = epkgs.trivialBuild {
    packageRequires = with epkgs; [
      # manual install until 25.05
      #
      # aidermacs
    ];
    pname = "tfl-aider";
    src = ./tfl-aider.el;
    version = "0";
  };
}
