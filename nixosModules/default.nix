inputs: {
  default = { lib, pkgs, ... }: {
    environment.systemPackages = [ inputs.home-manager.packages.${pkgs.system}.default ];
    home-manager.sharedModules = [ inputs.self.homeManagerModules.default ];
    imports = [
      ./acme.nix
      ./avahi.nix
      ./backlight.nix
      ./bluetooth.nix
      ./common.nix
      ./deploy-keys.nix
      ./desktop.nix
      ./emoji.nix
      ./fonts.nix
      ./forgejo.nix
      ./git.nix
      ./gotosocial.nix
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
      ./openvpn-proton.nix
      ./postgresql-backup.nix
      ./restic.nix
      ./ssh.nix
      ./sudo.nix
      ./suspend-when-ac-disconnected.nix
      ./systemd-notify-failure.nix
      ./tlp.nix
      ./tt-rss.nix
      ./tunnel.nix
      ./users.nix
      ./vaultwarden.nix
      ./webdav.nix
      ./xfce.nix
      ./yubikey.nix
      ./zsh.nix
      inputs.home-manager.nixosModules.home-manager
    ];
    nixpkgs.overlays = [
      inputs.self.overlays.default
      inputs.self.overlays.unstable
    ];
  };
}
