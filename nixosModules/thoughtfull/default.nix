{ agenix, home-manager, ... }@inputs:
{ lib, pkgs, ... }: {
  _module.args = {
    inherit (inputs) agenix home-manager unstable;
    thoughtfull = inputs.self;
  };
  boot.tmp.cleanOnBoot = lib.mkDefault true;
  console.useXkbConfig = lib.mkDefault true;
  environment.systemPackages = [
    agenix.packages.${pkgs.system}.default
    home-manager.packages.${pkgs.system}.default
  ];
  hardware.enableAllFirmware = lib.mkDefault true;
  imports = [
    ./acme.nix
    ./avahi.nix
    ./backlight.nix
    ./bluetooth.nix
    ./brother.nix
    ./deploy-keys.nix
    ./desktop.nix
    ./emoji.nix
    ./fonts.nix
    ./git.nix
    ./greek.nix
    ./home-manager.nix
    ./initrd-ssh.nix
    ./lock-screen.nix
    ./moonlander.nix
    ./nginx.nix
    ./nix.nix
    ./notify-reboot.nix
    ./nullmailer.nix
    ./openssh.nix
    ./overlay-thoughtfull.nix
    ./overlay-unstable.nix
    ./postgresql-backup.nix
    ./restic.nix
    ./root.nix
    ./ssh.nix
    ./sudo.nix
    ./suspend-when-ac-disconnected.nix
    ./systemd-notify-failure.nix
    ./tlp.nix
    ./tt-rss.nix
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
  i18n.defaultLocale = lib.mkDefault "en_US.UTF-8";
  networking.domain = lib.mkDefault "thoughtfull.systems";
  programs = {
    git.enable = lib.mkDefault true;
    zsh.enable = lib.mkDefault true;
  };
  services = {
    openssh.enable = lib.mkDefault true;
    xserver.xkb = {
      layout = lib.mkDefault "us";
      options = lib.mkDefault "grp:shifts_toggle,ctrl:nocaps,compose:rctrl";
      variant = lib.mkDefault "dvorak";
    };
  };
  users.mutableUsers = lib.mkDefault false;
}
