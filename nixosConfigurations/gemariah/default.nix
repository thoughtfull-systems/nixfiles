inputs : { lib, ... } : {
  boot.loader = {
    efi.canTouchEfiVariables = true;
    systemd-boot.enable = true;
  };
  home-manager.users.root.home.stateVersion = "24.11";
  imports = [
    ./hardware-configuration.nix
    ./technosophist.nix
    inputs.secrets.nixosModules.gemariah
    inputs.self.nixosModules.default
  ];
  networking.hostName = "gemariah";
  services.tlp.settings = {
    START_CHARGE_THRESH_BAT0 = lib.mkDefault 45;
    STOP_CHARGE_THRESH_BAT0 = lib.mkDefault 55;
  };
  # This option defines the first version of NixOS you have installed on this particular machine,
  # and is used to maintain compatibility with application data (e.g. databases) created on older NixOS versions.
  #
  # Most users should NEVER change this value after the initial install, for any reason,
  # even if you've upgraded your system to a new NixOS release.
  #
  # This value does NOT affect the Nixpkgs version your packages and OS are pulled from,
  # so changing it will NOT upgrade your system - see https://nixos.org/manual/nixos/stable/#sec-upgrading for how
  # to actually do that.
  #
  # This value being lower than the current NixOS release does NOT mean your system is
  # out of date, out of support, or vulnerable.
  #
  # Do NOT change this value unless you have manually inspected all the changes it would make to your configuration,
  # and migrated your data accordingly.
  #
  # For more information, see `man configuration.nix` or https://nixos.org/manual/nixos/stable/options#opt-system.stateVersion .
  system.stateVersion = "24.11";
  thoughtfull = {
    autoUpgrade = {
      flake = "github:thoughtfull-systems/nixfiles/nixos-24.11";
      inputs = [ "nixpkgs" ];
    };
    deploy-keys = [ { name = "nixfiles-secrets"; } ];
    desktop.enable = true;
    greek.enable = true;
  };
  time.timeZone = "America/New_York";
  users.mutableUsers = false;
}
