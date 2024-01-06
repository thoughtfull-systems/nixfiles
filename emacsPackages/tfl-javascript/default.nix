epkgs: {
  tfl-javascript = epkgs.trivialBuild {
    packageRequires = with epkgs; [
      js2-mode
      json-mode
      typescript-mode
    ];
    pname = "tfl-javascript";
    src = ./tfl-javascript.el;
    version = "0";
  };
}
