{ thoughtfull, ... }: {
  nixpkgs.overlays = [
    thoughtfull.overlays.default
    thoughtfull.overlays.unstable
  ];
}
