{ lib, ... }: {
  services.avahi = {
    enable = lib.mkDefault true;
    nssmdns4 = lib.mkDefault true;
    publish = {
      addresses = lib.mkDefault true;
      domain = lib.mkDefault true;
      enable = lib.mkDefault true;
    };
  };
}
