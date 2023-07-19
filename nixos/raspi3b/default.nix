{ config, lib, pkgs, thoughtfull, unstable, ... }: {
  boot = {
    cleanTmpDir = true;
    initrd = {
      availableKernelModules = [ "smsc95xx" "usb_storage" "usbhid" "xhci_pci" "xhci_hcd" ];
      kernelModules = [ "dm-snapshot" ];
      luks.devices.secure = {
        device = "/dev/sda";
        preLVM = true;
      };
      network = {
        enable = true;
        ssh = {
          authorizedKeys = config.users.users.paul.openssh.authorizedKeys.keys;
          enable = true;
          hostKeys = [
            /etc/ssh/initrd_ssh_host_ed25519_key
            /etc/ssh/initrd_ssh_host_rsa_key
          ];
          ignoreEmptyHostKeys = true;
          port = 222;
        };
      };
    };
    kernelPackages = pkgs.linuxPackages_latest;
    kernelParams = [
      "cma=32M"
      # AFAICT, this is required to get a luks password prompt
      "console=tty1"
    ];
    loader = {
      # generic-extlinux-compatible.enable = true;
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
  };
  console.useXkbConfig = true;
  documentation.nixos.enable = false;
  environment.systemPackages = [ pkgs.execline ];
  fileSystems = {
    "/" = {
      device = "/dev/disk/by-uuid/d78473e5-e415-40be-a0b7-a59f451b042f";
      fsType = "ext4";
      options = [ "noatime" "nodiratime" "discard" ];
    };
    "/boot" = {
      device = "/dev/disk/by-uuid/eaf656ae-fb96-4ebe-95a9-ce7dc991782b";
      fsType = "ext4";
      options = [ "noatime" "nodiratime" ];
    };
    "/boot/firmware" = {
      device = "/dev/disk/by-uuid/2178-694E";
      fsType = "vfat";
      options = [ "noatime" "nodiratime" ];
    };
    "/nix" = {
      device = "/dev/disk/by-uuid/8bff918b-d86d-4f26-b3ae-85b95e7225bc";
      fsType = "ext4";
      options = [ "noatime" "nodiratime" ];
    };
  };
  hardware.enableRedistributableFirmware = true;
  # home-manager = {
  #   extraSpecialArgs = { inherit nix-lib unstable; };
  # };
  i18n.defaultLocale = "en_US.UTF-8";
  imports = [
    thoughtfull.default
    ./paul.nix
    ./root.nix
  ];
  # my.restic.s3Bucket = "stadig-restic";
  thoughtfull.restic = {
    environmentFile = "/var/lib/restic/.env";
    passwordFile = "/var/lib/restic/passphrase";
    s3Bucket = "stadig-restic";
  };
  nix.gc.options = lib.mkForce "--delete-older-than 7d";
  networking = {
    domain = "stadig.name";
    firewall.allowedTCPPorts = [
      80
      443
      8384 # syncthing
    ];
    hostName = "raspi3b";
    interfaces.eth0 = {
      useDHCP = true;
      ipv4.addresses = [
        # My first-line router is on 192.168.1.x and will only NAT forward to that subnet.
        {
          address = "192.168.1.3";
          prefixLength = 24;
        }
      ];
    };
    # The global useDHCP flag is deprecated, therefore explicitly set to false here.
    # Per-interface useDHCP will be mandatory in the future, so this generated config
    # replicates the default behaviour.
    useDHCP = false;
  };
  powerManagement.cpuFreqGovernor = "ondemand";
  security.acme = {
    acceptTerms = true;
    certs."stadig.name".extraDomainNames = [
      "bw.stadig.name"
    ];
    defaults.email = "paul@stadig.name";
  };
  services = {
    nginx = {
      appendHttpConfig = ''
        # blocked IPs:
        # deny 73.177.192.154;
        allow all;

        # rate limiting: https://www.nginx.com/blog/rate-limiting-nginx/
        # allow list IPs subject to relaxed rate limit
        # map allow list IPs to 0, others to 1
        geo $limit {
          default 1;
          # allow list IPs:
          # 73.177.192.154/32 0;
        }

        # $limit_key is "" for allow list IPs, remote address for others
        map $limit $limit_key {
          0 "";
          1 $binary_remote_addr;
        }

        # allow list will match only the relaxed rate; others will match both,
        # and the more restrictive limit will apply
        limit_req_zone $limit_key          zone=limit_strict:1m  rate=20r/s;
        limit_req_zone $binary_remote_addr zone=limit_relaxed:1m rate=1000r/s;

        # Most websites have no more than 12 resources per load
        limit_req zone=limit_strict  burst=40   delay=20;
        limit_req zone=limit_relaxed burst=2000 nodelay;
        limit_req_status 429;
      '';
      enable = true;
      recommendedProxySettings = true;
      virtualHosts = {
        "bw.stadig.name" = {
          forceSSL = true;
          locations."/".proxyPass = "http://localhost:8000";
          useACMEHost = "stadig.name";
        };
        "stadig.name" = {
          enableACME = true;
          forceSSL = true;
        };
      };
    };
    openssh = {
      enable = true;
      passwordAuthentication = false;
    };
    syncthing.guiAddress = "0.0.0.0:8384";
    vaultwarden.enable = true;
    xserver = {
      layout = "dvorak";
      xkbOptions = "ctrl:nocaps";
    };
    zerotierone = {
      enable = true;
      joinNetworks = [ "af415e486f92a166" ];
    };
  };
  swapDevices = [{
    device = "/dev/disk/by-uuid/2dff7c80-387a-4e70-9fa3-9f8139c03b6f";
  }];
  system = {
    autoUpgrade = {
      dates = "12:00";
    };
    # This value determines the NixOS release from which the default
    # settings for stateful data, like file locations and database versions
    # on your system were taken. It‘s perfectly fine and recommended to leave
    # this value at the release version of the first install of this system.
    # Before changing this value read the documentation for this option
    # (e.g. man configuration.nix or on https://nixos.org/nixos/options.html).
    stateVersion = "21.11"; # Did you read the comment?
  };
  users.mutableUsers = false;
  virtualisation.docker.enable = true;
}
