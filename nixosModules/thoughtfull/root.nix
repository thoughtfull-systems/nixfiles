{ lib, pkgs, ... } : {
  services.openssh.settings.PermitRootLogin = lib.mkDefault "prohibit-password";
  users.users.root = lib.mkDefault {
    password = null;
    shell = pkgs.zsh;
  };
}
