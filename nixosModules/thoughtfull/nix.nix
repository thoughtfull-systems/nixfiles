# Table stakes nix configuration:
#   1. Turn on flakes
#   2. Enable nix store optimization
#   3. Enable nix store garbage collection
#   4. Enable autoUpgrade for nixpkgs
#
# For a private repo you must create a deploy key and use the appropriate hostname.  For exmaple, if
# your deploy key is named 'nixfiles', the flake should be something like
# 'git+ssh://git@nixfiles.github.com/...'.
{ config, lib, ... }: let
  desktop = config.thoughtfull.desktop.enable;
  cfg = config.thoughtfull.autoUpgrade;
in {
  options.thoughtfull.autoUpgrade = {
    flake = lib.mkOption {
      description = lib.mdDoc "Flake used for automatic upgrades.";
      type = lib.types.str;
    };
    inputs = lib.mkOption {
      description = lib.mdDoc "Flake inputs to update for upgrades.";
      type = lib.types.listOf lib.types.str;
    };
  };
  config = {
    nix = {
      gc = {
        automatic = lib.mkDefault true;
        dates = lib.mkDefault (if desktop then "12:15" else "03:15");
        options = lib.mkDefault "--delete-older-than 7d";
      };
      optimise = {
        automatic = lib.mkDefault true;
        dates = lib.mkDefault (if desktop then [ "12:30" ] else [ "03:30" ]);
      };
      settings = {
        auto-optimise-store = lib.mkDefault true;
        experimental-features = [ "flakes" "nix-command" ];
      };
    };
    system.autoUpgrade = {
      allowReboot = lib.mkDefault (!desktop);
      dates = lib.mkDefault (if desktop then "12:00" else "03:00");
      enable = lib.mkDefault true;
      flags = [ "--no-write-lock-file" "--refresh" ] ++
              (map (i: "--update-input ${i}") cfg.inputs);
      flake = cfg.flake;
    };
  };
}
