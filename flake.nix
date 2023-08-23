{
  description = "NixOS configuration";
  inputs = {
    agenix.url = "github:ryantm/agenix/e64961977f60388dd0b49572bb0fc453b871f896";
    hardware.url = "github:nixos/nixos-hardware/master";
    home-manager = {
      inputs.nixpkgs.follows = "nixpkgs";
      url = "github:thoughtfull-systems/home-manager/release-23.05";
    };
    nixpkgs.url = "github:thoughtfull-systems/nixpkgs/nixos-23.05";
    secrets.url = "git+ssh://git@github.com/thoughtfull-systems/nixfiles-secrets";
    # for some software I want the most recent version
    unstable.url = "github:nixos/nixpkgs/nixos-unstable";
  };
  outputs = { hardware, nixpkgs, secrets, ... }@inputs: rec {
    emacsPackages = import ./emacsPackages;
    homeManagerModules = import ./homeManagerModules;
    lib = import ./lib inputs;
    nixosModules = import ./nixosModules inputs;
    packages = lib.forAllSystems (system:
      import ./packages (import nixpkgs {
        config.allowUnfree = true;
        inherit system;
      })
    );
  };
}
