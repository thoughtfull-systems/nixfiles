{ config, ... } : {
  boot.initrd.network.ssh = {
    authorizedKeys = config.users.users.root.openssh.authorizedKeys.keys;
    port = 222;
  };
}
