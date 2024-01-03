epkgs: {
  tfl-javascript = epkgs.elpaBuild {
    packageRequires = with epkgs; [
      js2-mode
      json-mode
      typescript-mode
    ];
    pname = "tfl-javascript";
    src = ./tfl-javascript.el;
    version = "0.0.0";
  };
}
