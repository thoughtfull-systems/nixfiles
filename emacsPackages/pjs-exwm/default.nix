epkgs: {
  pjs-exwm = epkgs.trivialBuild {
    packageRequires = with epkgs; [
      exwm
      exwm-modeline
      pjs
    ];
    pname = "pjs-exwm";
    src = ./pjs-exwm.el;
    version = "0";
  };
}
