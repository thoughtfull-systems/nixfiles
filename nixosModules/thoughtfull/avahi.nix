{ lib, ... }: {
  services.avahi = lib.mkDefault {
    enable = true;
    nssmdns = true;
    publish = {
      addresses = true;
      domain = true;
      enable = true;
    };
  };
}
