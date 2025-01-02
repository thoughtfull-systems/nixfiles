{
  description = "NixOS configuration";
  inputs = {
    agenix = {
      inputs = {
        darwin.follows = "darwin";
        home-manager.follows = "home-manager";
        nixpkgs.follows = "nixpkgs";
      };
      url = "github:ryantm/agenix/main";
    };
    darwin = {
      url = "github:lnl7/nix-darwin/master";
      inputs.nixpkgs.follows = "nixpkgs";
    };
    home-manager = {
      inputs.nixpkgs.follows = "nixpkgs";
      url = "github:thoughtfull-systems/home-manager/release-24.11";
    };
    nixpkgs.url = "github:thoughtfull-systems/nixpkgs/nixos-24.11";
    secrets = {
      inputs = {
        agenix.follows = "agenix";
        darwin.follows = "darwin";
        home-manager.follows = "home-manager";
        nixpkgs.follows = "nixpkgs";
      };
      url = "git+ssh://git@github.com/thoughtfull-systems/nixfiles-secrets";
    };
    # for some software I want the most recent version
    unstable.url = "github:thoughtfull-systems/nixpkgs/nixpkgs-unstable";
  };
  outputs = inputs: {
    emacsPackages = import ./emacsPackages;
    homeManagerModules = import ./homeManagerModules;
    lib = import ./lib inputs;
    overlays = import ./overlays inputs;
    nixosConfigurations = import ./nixosConfigurations inputs;
    nixosModules = import ./nixosModules inputs;
    packages = import ./packages inputs;
  };
}
