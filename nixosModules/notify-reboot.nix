{ config, lib, pkgs, ... } : let
  cfg = config.thoughtfull.notify-reboot;
  desktop = config.thoughtfull.desktop.enable;
  readlink = "${pkgs.coreutils}/bin/readlink";
  check-for-reboot = pkgs.writeScriptBin "check-for-reboot" ''
    #!${pkgs.bash}/bin/bash
    booted="$(${readlink} /run/booted-system/{initrd,kernel,kernel-modules})"
    built="$(${readlink} /nix/var/nix/profiles/system/{initrd,kernel,kernel-modules})"
    if [ "''${booted}" = "''${built}" ]; then
      echo "System does not require a reboot"
      exit 1
    else
      echo $booted
      echo $built
      echo "System requires a reboot"
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
      default = null;
      description = lib.mdDoc "Email address from which to notify when reboot is required.";
      type = lib.types.str;
    };
    to = lib.mkOption {
      default = null;
      description = lib.mdDoc "Email address to notify when reboot is required.";
      type = lib.types.str;
    };
  };
  config = lib.mkIf cfg.enable {
    environment.systemPackages = [ check-for-reboot ];
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
        find = "${pkgs.findutils}/bin/find";
        sendmail = "${pkgs.nullmailer}/bin/sendmail";
        sudo = "${pkgs.sudo}/bin/sudo";
        file = "/tmp/thoughtfull-reboot-last-notified";
      in ''
        if ${check-for-reboot}/bin/check-for-reboot; then
          if [[ $(${find} ${file} -mmin -1440) != "${file}" ]]; then
            echo "Sending notification email"
            touch ${file}
            ${sudo} -u ${config.services.nullmailer.user} ${sendmail} -tf ${cfg.from} <<EOF
      From: ${cfg.from}
      To: ${cfg.to}
      Subject: [$HOST] requires a reboot

      $HOST requires a reboot.
      EOF
          else
            echo "Skipping notification email"
          fi
        fi
      '';
      serviceConfig.Type = "oneshot";
      startAt = lib.mkDefault "hourly";
      unitConfig.X-StopOnRemoval = false;
      wants = [ "network-online.target" ];
    };
  };
}
