epkgs: {
  pjs-org = epkgs.elpaBuild {
    packageRequires = with epkgs; [
      org-autolist
    ];
    pname = "pjs-org";
    src = ./pjs-org.el;
    version = "0.0.0";
  };
}
