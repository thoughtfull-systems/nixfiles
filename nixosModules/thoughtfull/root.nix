{ lib, pkgs, ... } : {
  home-manager.users.root = {
    home = {
      # This value determines the Home Manager release that your
      # configuration is compatible with. This helps avoid breakage
      # when a new Home Manager release introduces backwards
      # incompatible changes.
      #
      # You can update Home Manager without changing this value. See
      # the Home Manager release notes for a list of state version
      # changes in each release.
      stateVersion = lib.mkDefault "22.11";
    };
    thoughtfull.desktop.enable = lib.mkOverride 900 false;
  };
  services.openssh.settings.PermitRootLogin = lib.mkDefault "prohibit-password";
  users.users.root = lib.mkDefault {
    password = null;
    shell = pkgs.zsh;
  };
}
