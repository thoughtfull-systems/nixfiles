{ ... } : {
  programs.ssh = {
    extraConfig = ''
      Host unlock.*
        RequestTTY yes
        RemoteCommand cryptsetup-askpass
      Host *.local
        User root
        ForwardAgent yes
      Host *.lan
        User root
        ForwardAgent yes
    '';
    knownHosts = {
      "github-ecdsa" = {
        hostNames = [ "github.com" ];
        publicKey = builtins.readFile ./ssh/github-ecdsa.pub;
      };
      "github-ed25519" = {
        hostNames = [ "github.com" ];
        publicKey = builtins.readFile ./ssh/github-ed25519.pub;
      };
      "github-rsa" = {
        hostNames = [ "github.com" ];
        publicKey = builtins.readFile ./ssh/github-rsa.pub;
      };
    };
  };
}
