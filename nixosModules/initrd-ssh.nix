{ config, lib, ... } : {
  boot.initrd.network.ssh = {
    authorizedKeys = config.users.users.root.openssh.authorizedKeys.keys;
    port = lib.mkDefault 222;
  };
}
