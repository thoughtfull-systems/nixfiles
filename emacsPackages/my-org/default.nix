epkgs: {
  my-org = epkgs.elpaBuild {
    pname = "my-org";
    src = ./my-org.el;
    version = "0.0.0";
  };
}
