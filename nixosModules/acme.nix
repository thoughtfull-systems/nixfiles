{ lib, ... } : {
  security.acme.acceptTerms = lib.mkDefault true;
}
