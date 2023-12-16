{ lib, pkgs, ... } : {
  home-manager.users.root = {
    programs = {
      tmux.enable = lib.mkOverride 900 true;
      zsh.enable = lib.mkDefault true;
    };
    thoughtfull.desktop.enable = lib.mkOverride 900 false;
  };
  services.openssh.settings.PermitRootLogin = lib.mkDefault "prohibit-password";
  users.users.root = lib.mkDefault {
    password = lib.mkDefault null;
    shell = lib.mkDefault pkgs.zsh;
  };
}
