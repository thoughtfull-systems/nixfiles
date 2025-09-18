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
      ./gpg.nix
      ./home-manager.nix
      ./javascript.nix
      ./keychain.nix
      ./mcp.nix
      ./notifications.nix
      ./rust.nix
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
