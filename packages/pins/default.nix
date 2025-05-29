nixpkgs: nixpkgs.symlinkJoin {
  name = "pins";
  paths = [
    (nixpkgs.replaceVarsWith {
      dir = "bin";
      isExecutable = true;
      src = ./pins;
      replacements = {
        apg = "${nixpkgs.apg}/bin/apg";
      };
    })
  ];
}
