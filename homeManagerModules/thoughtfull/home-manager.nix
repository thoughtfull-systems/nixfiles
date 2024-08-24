{ lib, ... }: {
  # Let Home Manager install and manage itself.
  programs.home-manager.enable = lib.mkDefault true;
}
