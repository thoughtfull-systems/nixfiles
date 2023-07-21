{ thoughtfull, ... }: {
  home-manager.users.paul.imports = [ ../../home/raspi3b/paul.nix ];
  imports = [ thoughtfull.paul ];
  users.users.paul = {
    hashedPassword = "$6$FHwQvB6VtmKnyx3V$BNZLfh7N3Ja.YnVh8fPKrRsKmEM.APdUtJwp1/xhQFqJ/1FxgzRQE.V4qMOPOfMqCOV8T9GkkSRM3urafaSK70";
  };
}
