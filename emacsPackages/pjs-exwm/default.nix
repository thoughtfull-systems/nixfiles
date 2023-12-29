epkgs: {
  pjs-exwm = epkgs.elpaBuild {
    packageRequires = with epkgs; [
      exwm
      exwm-modeline
    ];
    pname = "pjs-exwm";
    src = ./pjs-exwm.el;
    version = "0.0.0";
  };
}
