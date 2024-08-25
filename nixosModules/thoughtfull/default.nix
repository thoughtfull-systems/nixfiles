{ agenix, home-manager, ... }@inputs:
{ lib, pkgs, ... }: {
  _module.args = {
    inherit (inputs) agenix home-manager unstable;
    thoughtfull = inputs.self;
  };
  environment.systemPackages = [
    agenix.packages.${pkgs.system}.default
    home-manager.packages.${pkgs.system}.default
  ];
  imports = [
    ./acme.nix
    ./avahi.nix
    ./backlight.nix
    ./bluetooth.nix
    ./brother.nix
    ./common.nix
    ./deploy-keys.nix
    ./desktop.nix
    ./emoji.nix
    ./fonts.nix
    ./git.nix
    ./greek.nix
    ./home-manager.nix
    ./initrd-ssh.nix
    ./keyboard.nix
    ./lock-screen.nix
    ./moonlander.nix
    ./nginx.nix
    ./nix.nix
    ./notify-reboot.nix
    ./nullmailer.nix
    ./openssh.nix
    ./overlays.nix
    ./postgresql-backup.nix
    ./restic.nix
    ./ssh.nix
    ./sudo.nix
    ./suspend-when-ac-disconnected.nix
    ./systemd-notify-failure.nix
    ./tlp.nix
    ./tt-rss.nix
    ./users.nix
    ./vaultwarden.nix
    ./vpn-home.nix
    ./vpn-proton.nix
    ./webdav.nix
    ./xfce.nix
    ./yubikey.nix
    ./zsh.nix
    agenix.nixosModules.default
    home-manager.nixosModules.home-manager
  ];
}
