inputs: {
  default = { lib, pkgs, ... }: {
    environment.systemPackages = [
      inputs.agenix.packages.${pkgs.system}.default
      inputs.home-manager.packages.${pkgs.system}.default
    ];
    home-manager = {
      sharedModules = [ inputs.self.homeManagerModules.default ];
    };
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
      inputs.agenix.nixosModules.default
      inputs.home-manager.nixosModules.home-manager
    ];
    nixpkgs.overlays = [
      inputs.self.overlays.default
      inputs.self.overlays.unstable
    ];
  };
}
