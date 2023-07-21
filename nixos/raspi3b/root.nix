{ config, thoughtfull, ... }: {
  home-manager.users.root.imports = [ ../../home/raspi3b/root.nix ];
  imports = [ thoughtfull.root ];
  users.users.root = {
    openssh.authorizedKeys.keys = config.users.users.paul.openssh.authorizedKeys.keys;
    password = null;
  };
}
