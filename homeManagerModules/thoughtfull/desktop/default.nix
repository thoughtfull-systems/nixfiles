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
      (zoom-us.overrideAttrs (_: {
        version = "5.16.10.668";
        src = fetchurl {
          url = "https://zoom.us/client/5.16.10.668/zoom_x86_64.pkg.tar.xz";
          hash = "sha256-dZQHbpvU8uNafmHtGoPhj6WsDhO20Dma/XwY6oa3Xes=";
        };
      }))
    ];
    fonts.fontconfig.enable = lib.mkForce true;
    services.blueman-applet.enable = lib.mkDefault osConfig.hardware.bluetooth.enable;
  };
  imports = [
    ./discord.nix
    ./firefox.nix
    ./obsidian.nix
    ./slack.nix
    ./xbanish.nix
    ./xfce.nix
  ];
}
