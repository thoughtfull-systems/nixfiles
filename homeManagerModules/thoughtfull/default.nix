{ ... }: {
  imports = [
    ./clojure
    ./desktop
    ./emacs
    ./emoji.nix
    ./exwm
    ./git.nix
    ./gnome-terminal.nix
    ./home-manager.nix
    ./javascript.nix
    ./keychain.nix
    ./notifications.nix
    ./overlay-emacs.nix
    ./autostart.nix
    ./starship.nix
    ./syncthing.nix
    ./tmux.nix
    ./xss-lock.nix
    ./yubikey.nix
    ./zsh.nix
  ];
  systemd.user.startServices = "sd-switch";
}
