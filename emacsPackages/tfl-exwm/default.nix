epkgs: {
  tfl-exwm = epkgs.trivialBuild {
    packageRequires = with epkgs; [
      exwm
      exwm-modeline
      tfl
      tfl-gtd
    ];
    pname = "tfl-exwm";
    src = ./src;
    version = "0";
  };
}
