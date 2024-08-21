{ config, lib, pkgs, ... } : let
  cfg = config.thoughtfull.notify-reboot;
  desktop = config.thoughtfull.desktop.enable;
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
    services.nullmailer.enable = true;
    systemd.services.notify-reboot = {
      after = [ "network-online.target" ];
      description = "Notify when system upgrade requires a reboot";
      enable = true;
      environment = {
        HOST = "%H";
      };
      restartIfChanged = false;
      script = let
        readlink = "${pkgs.coreutils}/bin/readlink";
        sendmail = "${pkgs.nullmailer}/bin/sendmail";
        sudo = "${pkgs.sudo}/bin/sudo";
      in ''
        booted="$(${readlink} /run/booted-system/{initrd,kernel,kernel-modules})"
        built="$(${readlink} /nix/var/nix/profiles/system/{initrd,kernel,kernel-modules})"
        if [ "''${booted}" = "''${built}" ]; then
          echo "$HOST does not require a reboot"
        else
          echo "$HOST requires a reboot"
          ${sudo} -u ${config.services.nullmailer.user} ${sendmail} -tf ${cfg.from} <<EOF
      From: ${cfg.from}
      To: ${cfg.to}
      Subject: [$HOST] requires a reboot

      $HOST requires a reboot.
      EOF
        fi
      '';
      serviceConfig.Type = "oneshot";
      startAt = lib.mkDefault (if desktop then "13:15" else "04:15");
      unitConfig.X-StopOnRemoval = false;
      wants = [ "network-online.target" ];
    };
  };
}
