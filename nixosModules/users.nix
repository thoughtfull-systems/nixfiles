{ lib, pkgs, ... } : {
  home-manager.users.root = {
    programs = {
      # I want tmux for root, but it follows desktop and I'm explicitly setting desktop to false, so
      # I'm overriding.
      tmux.enable = lib.mkOverride 900 true;
      zsh.enable = lib.mkDefault true;
    };
    thoughtfull.desktop.enable = lib.mkOverride 900 false;
  };
  services.openssh.settings.PermitRootLogin = lib.mkDefault "prohibit-password";
  users = {
    mutableUsers = lib.mkDefault false;
    users.root = {
      password = lib.mkDefault null;
      # NixOS sets a default, so I'm (slightly) overriding it.
      shell = lib.mkOverride 900 pkgs.zsh;
    };
  };
}
