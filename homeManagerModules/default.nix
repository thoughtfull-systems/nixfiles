{
  default = { lib, ... }: {
    imports = [
      ./clojure.nix
      ./desktop
      ./emacs/gtd.nix
      ./emoji.nix
      ./exwm.nix
      ./git.nix
      ./gnome-terminal.nix
      ./gpg.nix
      ./home-manager.nix
      ./javascript.nix
      ./keychain.nix
      ./notifications.nix
      ./starship.nix
      ./syncthing.nix
      ./tmux.nix
      ./tpm.nix
      ./yubikey.nix
      ./zsh.nix
    ];
    systemd.user.startServices = lib.mkDefault "sd-switch";
  };
}
