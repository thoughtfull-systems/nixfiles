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
    # for some software I want the most recent version
    unstable.url = "github:nixos/nixpkgs/nixos-unstable";
  };
  outputs = { hardware, nixpkgs, ... }@inputs: rec {
    emacsPackages = import ./emacsPackages;
    homeManagerModules = import ./homeManagerModules;
    lib = import ./lib inputs;
    nixosConfigurations = {
      hemera = nixpkgs.lib.nixosSystem {
        modules = [
          hardware.nixosModules.lenovo-thinkpad-x13
          ./nixos/hemera
        ];
        specialArgs.thoughtfull = nixosModules;
        system = "x86_64-linux";
      };
      raspi3b = nixpkgs.lib.nixosSystem {
        modules = [ ./nixos/raspi3b ];
        specialArgs.thoughtfull = nixosModules;
        system = "aarch64-linux";
      };
      ziph = nixpkgs.lib.nixosSystem {
        modules = [ ./nixos/ziph ];
        specialArgs.thoughtfull = nixosModules;
        system = "x86_64-linux";
      };
    };
    nixosModules = import ./nixosModules inputs;
    packages = lib.forAllSystems (system:
      import ./packages (import nixpkgs {
        config.allowUnfree = true;
        inherit system;
      })
    );
  };
}
