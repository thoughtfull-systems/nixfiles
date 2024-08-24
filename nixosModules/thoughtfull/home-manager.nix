{ lib, pkgs, secrets, thoughtfull, ... }: {
  home-manager = {
    extraSpecialArgs = {
      inherit secrets;
      thoughtfull = thoughtfull.homeManagerModules;
    };
    sharedModules = [ thoughtfull.homeManagerModules.default ];
    useGlobalPkgs = lib.mkDefault true;
    useUserPackages = lib.mkDefault true;
  };
}
