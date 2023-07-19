{ thoughtfull, ... }: {
  home-manager.users.root.imports = [ ../../home/raspi3b/root.nix ];
  imports = [ thoughtfull.root ];
  # imports = [
  #   ../common/home-manager.nix
  # ];
  users.users.root.openssh.authorizedKeys.keys = [
    "ecdsa-sha2-nistp521 AAAAE2VjZHNhLXNoYTItbmlzdHA1MjEAAAAIbmlzdHA1MjEAAACFBAB2hfargabOYq6TZ+U6zXUZG+SWrxWdV0Fq5AbhDLghAL4kdwi1j5Q9C8ki622ZwIkk+v7+575IXgyezlHIHjIFFwDf09ODfTPVSwNizpRBK8uMX1YV0XpULJmV8nOJFF0gbn9gQNktM6Obfuhl7QBGhmpEvnvROsBaAqU8OqcQeMRg+w== paul@carbon"
    "ssh-ed25519 AAAAC3NzaC1lZDI1NTE5AAAAIK4ztYeWkCSPNWnSxiqxx49qeP1uzibyj15rRCWgoLJb paul@hemera.stadig.name"
  ];
}
