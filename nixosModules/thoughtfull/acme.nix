{ lib, ... } : {
  security.acme = {
    acceptTerms = true;
    defaults.email = lib.mkDefault "paul@thoughtfull.systems";
  };
}
