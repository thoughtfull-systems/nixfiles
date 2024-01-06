{ config, lib, osConfig, pkgs, ... }: let
  cfg = config.thoughtfull.desktop;
in {
  options.thoughtfull.desktop.enable = lib.mkEnableOption "desktop";
  config = lib.mkIf cfg.enable {
    home.packages = with pkgs; [
      flameshot
      libreoffice
      notify-desktop
      tor-browser-bundle-bin
      unstable.zoom-us
    ];
    fonts.fontconfig.enable = lib.mkForce true;
    services.blueman-applet.enable = lib.mkDefault osConfig.hardware.bluetooth.enable;
  };
  imports = [
    ./firefox.nix
    ./obsidian.nix
    ./xbanish.nix
    ./xfce.nix
  ];
}
