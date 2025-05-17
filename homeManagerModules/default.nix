{
  default = { lib, ... }: {
    imports = [
      ./aider.nix
      ./autostart.nix
      ./clojure.nix
      ./desktop
      ./emacs.nix
      ./emacs/gtd.nix
      ./emoji.nix
      ./exwm.nix
      ./git.nix
      ./gnome-terminal.nix
      ./home-manager.nix
      ./javascript.nix
      ./keychain.nix
      ./notifications.nix
      ./overlay-emacs.nix
      ./rust.nix
      ./starship.nix
      ./syncthing.nix
      ./tmux.nix
      ./yubikey.nix
      ./zsh.nix
    ];
    systemd.user.startServices = lib.mkDefault "sd-switch";
  };
}
