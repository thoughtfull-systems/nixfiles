{ pkgs, thoughtfull, ... }: {
  boot.loader = {
    efi.canTouchEfiVariables = true;
    systemd-boot.enable = true;
  };
  hardware = {
    acpilight.enable = true;
    bluetooth.enable = true;
    ledger.enable = true;
    opengl.enable = true;
  };
  imports = [
    ./filesystems.nix
    ./hardware-configuration.nix
    ./paul.nix
    ./root.nix
    thoughtfull.default
  ];
  networking.hostName = "hemera";
  programs = {
    ssh.extraConfig = ''
      Host *.local
        ForwardAgent yes
      Host raspi3b.lan
        ForwardAgent Yes
        Hostname raspi3b.lan
        User root
      Host raspi3b
        ForwardAgent Yes
        Hostname raspi3b.lan
        RemoteCommand tmux att
        RequestTTY yes
        User root
      Host raspi3b.unlock
        ForwardAgent Yes
        Hostname raspi3b.lan
        Port 222
        RemoteCommand cryptsetup-askpass
        RequestTTY yes
        User root
    '';
  };
  services = {
    openssh.enable = true;
    trezord.enable = true;
  };
  system.stateVersion = "22.05";
  thoughtfull.desktop.enable = true;
  thoughtfull.vpn.home.enable = true;
  virtualisation.libvirtd.enable = true;
}
