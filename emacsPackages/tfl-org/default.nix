epkgs: {
  tfl-org = epkgs.elpaBuild {
    packageRequires = with epkgs; [
      org-autolist
    ];
    pname = "tfl-org";
    src = ./tfl-org.el;
    version = "0.0.0";
  };
}
