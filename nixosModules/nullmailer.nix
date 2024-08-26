{ config, lib, ... } : let
  cfg = config.thoughtfull.nullmailer;
in {
  options.thoughtfull.nullmailer.age = {
    remotesFile = lib.mkOption {
      description = "Encrypted age file containing nullmailer remotes";
      type = lib.types.path;
    };
  };
  config = lib.mkIf config.services.nullmailer.enable {
    age.secrets.thoughtfull-nullmailer-remotes = {
      file = cfg.age.remotesFile;
      owner = config.services.nullmailer.user;
    };
    services.nullmailer = {
      remotesFile = config.age.secrets.thoughtfull-nullmailer-remotes.path;
    };
  };
}
