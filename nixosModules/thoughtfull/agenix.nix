agenix: { pkgs, ... } : {
  environment.systemPackages = [
    agenix.packages.${pkgs.system}.default
  ];
  imports = [
    agenix.nixosModules.default
  ];
}
