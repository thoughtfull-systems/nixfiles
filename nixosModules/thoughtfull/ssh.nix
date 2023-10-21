{ ... } : {
  programs.ssh = {
    extraConfig = ''
      Host unlock.*
      RemoteCommand cryptsetup-askpass
      RequestTTY yes
      User root
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
