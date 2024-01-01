{ ... }: {
  imports = [
    ./clojure.nix
    ./desktop
    ./emacs
    ./emoji.nix
    ./exwm.nix
    ./git.nix
    ./gnome-terminal.nix
    ./home-manager.nix
    ./javascript.nix
    ./keychain.nix
    ./notifications.nix
    ./overlay-emacs.nix
    ./pjs-autostart.nix
    ./starship.nix
    ./syncthing.nix
    ./tmux.nix
    ./xss-lock.nix
    ./yubikey.nix
    ./zsh.nix
  ];
  systemd.user.startServices = "sd-switch";
}
