home-manager: { lib, pkgs, secrets, thoughtfull, ... }: {
  environment.systemPackages = [
    pkgs.home-manager
  ];
  home-manager = {
    extraSpecialArgs = {
      inherit secrets;
      thoughtfull = thoughtfull.home;
    };
    sharedModules = [ thoughtfull.home.thoughtfull ];
    useGlobalPkgs = lib.mkDefault true;
    useUserPackages = lib.mkDefault true;
  };
  imports = [
    home-manager.nixosModules.home-manager
  ];
}
