{ config, lib, pkgs, ... } : let
  enabled = config.security.tpm2.enable;
in lib.mkIf enabled
  {
    security.tpm2 = {
      pkcs11.enable = enabled;
      tctiEnvironment.enable = enabled;
    };
  }
