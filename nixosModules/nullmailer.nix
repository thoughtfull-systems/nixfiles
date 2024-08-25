{ config, lib, ... } : {
  services.nullmailer = {
    config.adminaddr = lib.mkDefault "technosophist@thoughtfull.systems";
    remotesFile = config.age.secrets.nullmailer-remotes.path;
  };
}
