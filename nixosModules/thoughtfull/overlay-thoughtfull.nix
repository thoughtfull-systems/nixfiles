{ ... }: {
  nixpkgs.overlays = [ (final: prev: { thoughtfull = import ../../packages final; }) ];
}
