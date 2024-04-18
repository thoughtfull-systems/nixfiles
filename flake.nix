{
  description = "NixOS configuration";
  inputs = {
    agenix = {
      inputs.nixpkgs.follows = "nixpkgs";
      url = "github:ryantm/agenix/e64961977f60388dd0b49572bb0fc453b871f896";
    };
    home-manager = {
      inputs.nixpkgs.follows = "nixpkgs";
      url = "github:thoughtfull-systems/home-manager/release-23.11";
    };
    nixpkgs.url = "github:thoughtfull-systems/nixpkgs/nixos-23.11";
    # for some software I want the most recent version
    unstable.url = "github:thoughtfull-systems/nixpkgs/nixpkgs-unstable";
  };
  outputs = { nixpkgs, self, ... }@inputs: {
    emacsPackages = import ./emacsPackages;
    homeManagerModules = import ./homeManagerModules;
    lib = import ./lib inputs;
    nixosModules = import ./nixosModules inputs;
    packages = self.lib.forAllSystems (system:
      import ./packages (import nixpkgs {
        config.allowUnfree = true;
        inherit system;
      })
    );
  };
}
