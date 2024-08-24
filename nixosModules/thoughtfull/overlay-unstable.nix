{ unstable, ... }: {
  nixpkgs.overlays = [
    (final: prev: {
      unstable = import unstable {
        config = final.config;
        system = final.system;
      };
    })
  ];
}
