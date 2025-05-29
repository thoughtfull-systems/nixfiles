nixpkgs: nixpkgs.symlinkJoin {
  name = "un";
  paths = [
    (nixpkgs.replaceVarsWith {
      dir = "bin";
      isExecutable = true;
      replacements = {
        apg = "${nixpkgs.apg}/bin/apg";
      };
      src = ./uns;
    })
  ];
}
