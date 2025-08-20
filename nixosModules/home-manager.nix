{ lib, pkgs, ... }: {
  home-manager = {
    sharedModules = [{
      programs.emacs.overrides = (final: prev:
        (import ../emacsPackages final) //
        { unstable = pkgs.unstable.emacsPackages; }
      );
    }];
    useGlobalPkgs = lib.mkDefault true;
    useUserPackages = lib.mkDefault true;
  };
}
