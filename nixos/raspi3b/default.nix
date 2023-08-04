{ config, lib, pkgs, secrets, thoughtfull, unstable, ... }: {
  boot = {
    initrd = {
      availableKernelModules = [ "smsc95xx" "usbhid" "xhci_pci" "xhci_hcd" ];
      kernelModules = [ "dm-snapshot" ];
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
      80
      443
      8384 # syncthing
    ];
    hostName = "raspi3b";
  };
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
    openssh.enable = true;
    vaultwarden.enable = true;
    zerotierone = {
      enable = true;
      joinNetworks = [ "af415e486f92a166" ];
    };
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
    restic = {
      environmentFile = "/var/lib/restic/.env";
      passwordFile = "/var/lib/restic/passphrase";
      s3Bucket = "stadig-restic";
    };
  };
  virtualisation.docker.enable = true;
}
