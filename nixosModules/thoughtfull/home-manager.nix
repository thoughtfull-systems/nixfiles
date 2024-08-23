home-manager: thoughtfull: { lib, pkgs, secrets, ... }: let
  home = thoughtfull.nixosModules.home;
in {
  environment.systemPackages = [
    pkgs.home-manager
  ];
  home-manager = {
    extraSpecialArgs = {
      inherit secrets;
      thoughtfull = home;
    };
    sharedModules = [ home.thoughtfull ];
    useGlobalPkgs = lib.mkDefault true;
    useUserPackages = lib.mkDefault true;
  };
  imports = [
    home-manager.nixosModules.home-manager
  ];
}
