nixpkgs: nixpkgs.symlinkJoin {
  name = "pins";
  paths = [
    (nixpkgs.substituteAll {
      dir = "bin";
      isExecutable = true;
      src = ./pins;
      apg = "${nixpkgs.apg}/bin/apg";
    })
  ];
}
