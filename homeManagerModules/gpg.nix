{ config, lib, pkgs, ... }: {
  home.packages = [ pkgs.pinentry-gtk2 ];
  programs.gpg.enable = true;
  services.gpg-agent = {
    enable = true;
    enableScDaemon = true;
    pinentry = {
      package = pkgs.pinentry-gtk2;
      program = "pinentry";
    };
  };
}
