epkgs: {
  my-javascript-dev = epkgs.elpaBuild {
    packageRequires = with epkgs; [
      my-prog
      typescript-mode
    ];
    pname = "my-javascript-dev";
    src = ./my-javascript-dev.el;
    version = "0.0.0";
  };
}
