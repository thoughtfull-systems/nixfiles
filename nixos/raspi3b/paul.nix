{ ... }: {
  home-manager.users.paul.imports = [ ../../home/raspi3b/paul.nix ];
  users.users.paul = {
    extraGroups = [ "wheel" ];
    group = "users";
    hashedPassword = "$6$FHwQvB6VtmKnyx3V$BNZLfh7N3Ja.YnVh8fPKrRsKmEM.APdUtJwp1/xhQFqJ/1FxgzRQE.V4qMOPOfMqCOV8T9GkkSRM3urafaSK70";
    isNormalUser = true;
    openssh.authorizedKeys.keys = [
      "ecdsa-sha2-nistp521 AAAAE2VjZHNhLXNoYTItbmlzdHA1MjEAAAAIbmlzdHA1MjEAAACFBAB2hfargabOYq6TZ+U6zXUZG+SWrxWdV0Fq5AbhDLghAL4kdwi1j5Q9C8ki622ZwIkk+v7+575IXgyezlHIHjIFFwDf09ODfTPVSwNizpRBK8uMX1YV0XpULJmV8nOJFF0gbn9gQNktM6Obfuhl7QBGhmpEvnvROsBaAqU8OqcQeMRg+w== paul@carbon"
      "ssh-ed25519 AAAAC3NzaC1lZDI1NTE5AAAAIK4ztYeWkCSPNWnSxiqxx49qeP1uzibyj15rRCWgoLJb paul@hemera.stadig.name"
    ];
    uid = 1000;
  };
}
