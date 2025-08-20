epkgs: {
  tfl-mcp = epkgs.trivialBuild {
    packageRequires = with epkgs; [
      unstable.eca
      unstable.gptel
      unstable.mcp
    ];
    pname = "tfl-mcp";
    src = ./src;
    version = "0";
  };
}
