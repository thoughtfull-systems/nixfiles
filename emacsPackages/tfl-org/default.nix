epkgs: {
  tfl-org = epkgs.trivialBuild {
    packageRequires = with epkgs; [
      org
      org-autolist
    ];
    pname = "tfl-org";
    src = ./tfl-org.el;
    version = "0";
  };
}
