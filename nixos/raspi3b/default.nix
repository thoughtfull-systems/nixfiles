{ config, pkgs, secrets, thoughtfull, ... }: {
  age.secrets = {
    restic-env.file = secrets.age.restic-env;
    restic-passphrase.file = secrets.age.restic-passphrase;
  };
  boot = {
    initrd = {
      availableKernelModules = [
        # allows moonlander, but only if pressing a key during detection
        "usbhid"
        # nic for initrd openssh
        "smsc95xx"
      ];
      luks.devices.secure = {
        device = "/dev/sda";
        preLVM = true;
      };
      network = {
        enable = true;
        ssh = {
          authorizedKeys = config.users.users.root.openssh.authorizedKeys.keys;
          enable = true;
          hostKeys = [
            secrets.initrd_ssh_host_ed25519_key
            secrets.initrd_ssh_host_rsa_key
          ];
          port = 222;
        };
      };
    };
    kernelParams = [
      "cma=32M"
      # AFAICT, this is required to get a luks password prompt
      "console=tty1"
    ];
    loader = {
      grub.enable = false;
      raspberryPi = {
        enable = true;
        firmwareConfig = ''
          hdmi_force_hotplug=1
        '';
        uboot.enable = true;
        version = 3;
      };
    };
    tmp.cleanOnBoot = true;
  };
  environment.systemPackages = [ pkgs.execline ];
  imports = [
    ./filesystems.nix
    ./hardware-configuration.nix
    ./paul.nix
    ./root.nix
    thoughtfull.default
  ];
  networking = {
    firewall.allowedTCPPorts = [
      8384 # syncthing
    ];
    hostName = "raspi3b";
  };
  services = {
    nginx.enable = true;
    vaultwarden.enable = true;
  };
  system = {
    autoUpgrade.flags = [
      "--override-input secrets git+ssh://git@nixfiles-secrets.github.com/thoughtfull-systems/nixfiles-secrets"
    ];
    # This value determines the NixOS release from which the default
    # settings for stateful data, like file locations and database versions
    # on your system were taken. It‘s perfectly fine and recommended to leave
    # this value at the release version of the first install of this system.
    # Before changing this value read the documentation for this option
    # (e.g. man configuration.nix or on https://nixos.org/nixos/options.html).
    stateVersion = "21.11"; # Did you read the comment?
  };
  thoughtfull = {
    deploy-keys = [ { name = "nixfiles-secrets"; } ];
    nginx.proxies."bw.stadig.name".backend = "http://localhost:8000";
    restic = {
      environmentFile = config.age.secrets.restic-env.path;
      passwordFile = config.age.secrets.restic-passphrase.path;
      s3Bucket = "stadig-restic";
    };
  };
  virtualisation.docker.enable = true;
}
