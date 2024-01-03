epkgs: {
  tfl-exwm = epkgs.trivialBuild {
    packageRequires = with epkgs; [
      exwm
      exwm-modeline
      tfl
    ];
    pname = "tfl-exwm";
    src = ./tfl-exwm.el;
    version = "0";
  };
}
