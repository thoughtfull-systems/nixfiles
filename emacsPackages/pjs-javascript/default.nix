epkgs: {
  pjs-javascript = epkgs.elpaBuild {
    packageRequires = with epkgs; [
      js2-mode
      json-mode
      typescript-mode
    ];
    pname = "pjs-javascript";
    src = ./pjs-javascript.el;
    version = "0.0.0";
  };
}
