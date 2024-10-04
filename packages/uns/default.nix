nixpkgs: nixpkgs.symlinkJoin {
  name = "un";
  paths = [
    (nixpkgs.substituteAll {
      dir = "bin";
      isExecutable = true;
      src = ./uns;
      apg = "${nixpkgs.apg}/bin/apg";
    })
  ];
}
