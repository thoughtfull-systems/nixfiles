{
  description = "Nix/NixOS/Home Manager files";
  inputs = {
    home-manager = {
      inputs.nixpkgs.follows = "nixpkgs";
      url = "github:thoughtfull-nix/home-manager/release-25.11";
    };
    nixpkgs.url = "github:thoughtfull-nix/nixpkgs/nixos-25.11";
    # for some software I want the most recent version
    unstable.url = "github:thoughtfull-nix/nixpkgs/nixos-unstable";
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
