{ config, lib, ... } : {
  services.nullmailer = {
    remotesFile = config.age.secrets.nullmailer-remotes.path;
  };
}
