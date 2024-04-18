home-manager: thoughtfull: { lib, pkgs, secrets, ... }: {
  environment.systemPackages = [
    pkgs.home-manager
  ];
  home-manager = {
    extraSpecialArgs = {
      inherit secrets;
      thoughtfull = thoughtfull.nixosModules.home;
    };
    sharedModules = [ thoughtfull.nixosModules.home.thoughtfull ];
    useGlobalPkgs = lib.mkDefault true;
    useUserPackages = lib.mkDefault true;
  };
  imports = [
    home-manager.nixosModules.home-manager
  ];
}
