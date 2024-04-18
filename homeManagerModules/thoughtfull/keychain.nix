{ lib, ... }: {
  programs.keychain = {
    agents = [ "gpg,ssh" ];
    enable = lib.mkDefault true;
    extraFlags = [ "--nogui" "--systemd" "-q" ];
    inheritType = lib.mkDefault "any-once";
    keys = [ "id_ed25519" ];
  };
}
