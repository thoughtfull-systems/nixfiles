{ config, lib, pkgs, ... } : let
  enabled = config.security.tpm2.enable;
in lib.mkIf enabled
  {
    environment.systemPackages = lib.mkIf enabled [ pkgs.tpm2-tools ];
    security.tpm2 = {
      pkcs11.enable = enabled;
      tctiEnvironment.enable = enabled;
    };
  }
