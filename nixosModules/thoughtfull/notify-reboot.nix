{ config, lib, pkgs, ... } : let
  cfg = config.thoughtfull.notify-reboot;
  desktop = config.thoughtfull.desktop.enable;
  readlink = "${pkgs.coreutils}/bin/readlink";
  reboot-required = pkgs.writeScriptBin "reboot-required" ''
    #!${pkgs.bash}/bin/bash
    booted="$(${readlink} /run/booted-system/{initrd,kernel,kernel-modules})"
    built="$(${readlink} /nix/var/nix/profiles/system/{initrd,kernel,kernel-modules})"
    if [ "''${booted}" = "''${built}" ]; then
      echo "Reboot not required"
      exit 1
    else
      echo $booted
      echo $built
      echo "Reboot required"
      exit 0
    fi
  '';
in {
  options.thoughtfull.notify-reboot = {
    enable = lib.mkOption {
      default = !desktop;
      description = lib.mdDoc "Notify when system upgrade requires a reboot.";
      type = lib.types.bool;
    };
    from = lib.mkOption {
      default = "technosophist@thoughtfull.systems";
      description = lib.mdDoc "Email address from which to notify when reboot is required.";
      type = lib.types.str;
    };
    to = lib.mkOption {
      default = "technosophist@thoughtfull.systems";
      description = lib.mdDoc "Email address to notify when reboot is required.";
      type = lib.types.str;
    };
  };
  config = lib.mkIf cfg.enable {
    environment.systemPackages = [ reboot-required ];
    services.nullmailer = {
      enable = true;
      setSendmail = true;
    };
    systemd.services.notify-reboot = {
      after = [ "network-online.target" ];
      description = "Notify when system upgrade requires a reboot";
      enable = true;
      environment = {
        HOST = "%H";
      };
      restartIfChanged = false;
      script = let
        sendmail = "${pkgs.nullmailer}/bin/sendmail";
        sudo = "${pkgs.sudo}/bin/sudo";
      in ''
        if ${reboot-required}/bin/reboot-required; then
          ${sudo} -u ${config.services.nullmailer.user} ${sendmail} -tf ${cfg.from} <<EOF
      From: ${cfg.from}
      To: ${cfg.to}
      Subject: [$HOST] requires a reboot

      $HOST requires a reboot.
      EOF
        fi
      '';
      serviceConfig.Type = "oneshot";
      startAt = lib.mkDefault (if desktop then "12:45" else "03:45");
      unitConfig.X-StopOnRemoval = false;
      wants = [ "network-online.target" ];
    };
  };
}
