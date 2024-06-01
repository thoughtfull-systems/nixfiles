{ lib, ... }: {
  services.avahi = lib.mkDefault {
    enable = true;
    nssmdns4 = true;
    publish = {
      addresses = true;
      domain = true;
      enable = true;
    };
  };
}
