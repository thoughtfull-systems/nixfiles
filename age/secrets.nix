with builtins;
let
  hemera = readFile ./keys/hemera.pub;
  raspi3b = readFile ./keys/raspi3b.pub;
  yubikey = readFile ./keys/yk5nano475.pub;
  ziph = readFile ./keys/ziph.pub;
  all = [
    hemera
    raspi3b
    yubikey
    ziph
  ];
in
{
  "secrets/vpn-proton-auth-user-pass.age".publicKeys = all;
  "secrets/vpn-proton-config.age".publicKeys = all;
  "secrets/vpn-home-askpass.age".publicKeys = all;
  "secrets/vpn-home-config.age".publicKeys = all;
}
