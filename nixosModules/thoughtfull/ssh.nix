{ ... } : {
  programs.ssh.knownHosts = {
    "hemera.lan".publicKey = builtins.readFile ./ssh/hemera.pub;
    "raspi3b.lan".publicKey = builtins.readFile ./ssh/raspi3b.pub;
  };
}
