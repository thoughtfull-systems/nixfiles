{ secrets, ... } : {
  programs.ssh.knownHosts = {
    "hemera.lan".publicKey = secrets.host-keys.hemera;
    "raspi3b.lan".publicKey = secrets.host-keys.raspi3b;
  };
}
