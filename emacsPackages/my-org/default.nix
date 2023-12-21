epkgs: {
  my-org = epkgs.elpaBuild {
    packageRequires = with epkgs; [
      org-autolist
    ];
    pname = "my-org";
    src = ./my-org.el;
    version = "0.0.0";
  };
}
