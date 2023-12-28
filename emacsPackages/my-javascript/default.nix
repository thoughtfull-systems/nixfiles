epkgs: {
  my-javascript = epkgs.elpaBuild {
    packageRequires = with epkgs; [
      js2-mode
      json-mode
      typescript-mode
    ];
    pname = "my-javascript";
    src = ./my-javascript.el;
    version = "0.0.0";
  };
}
