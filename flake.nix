{
  description = "Nix/NixOS/Home Manager files";
  inputs = {
    home-manager = {
      inputs.nixpkgs.follows = "nixpkgs";
      url = "github:thoughtfull-systems/home-manager/release-24.11";
    };
    nixpkgs.url = "github:thoughtfull-systems/nixpkgs/nixos-24.11";
    # for some software I want the most recent version
    unstable.url = "github:thoughtfull-systems/nixpkgs/nixpkgs-unstable";
  };
  outputs = inputs: {
    emacsPackages = import ./emacsPackages;
    homeManagerModules = import ./homeManagerModules;
    lib = import ./lib inputs;
    overlays = import ./overlays inputs;
    nixosModules = import ./nixosModules inputs;
    packages = import ./packages inputs;
  };
}
