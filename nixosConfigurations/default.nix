inputs : {
  gemariah = inputs.nixpkgs.lib.nixosSystem {
    modules = [
      (import ./gemariah inputs)
    ];
    system = "x86_64-linux";
  };
}
