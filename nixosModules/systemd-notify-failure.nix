{ config, lib, pkgs, ... } : let
  cfg = config.thoughtfull.systemd-notify-failure;
in {
  options.thoughtfull.systemd-notify-failure = {
    enable = lib.mkOption {
      default = !config.thoughtfull.desktop.enable;
      description = lib.mdDoc "Notify on systemd service failures.";
      type = lib.types.bool;
    };
    from = lib.mkOption {
      default = null;
      description = lib.mdDoc "Email address from which to notify on systemd service failures.";
      type = lib.types.str;
    };
    to = lib.mkOption {
      default = null;
      description = lib.mdDoc "Email address to notify on systemd service failures.";
      type = lib.types.str;
    };
    services = lib.mkOption {
      default = [];
      description = lib.mdDoc "";
      type = lib.types.listOf lib.types.str;
    };
  };
  config = lib.mkIf cfg.enable {
    services.nullmailer = {
      enable = true;
      setSendmail = true;
    };
    systemd.services = {
      "notify-failure@" = {
        enable = true;
        environment = {
          UNIT = "%i";
          HOST = "%H";
        };
        description = "Failure notification for %i";
        script = let
          sendmail = "${pkgs.nullmailer}/bin/sendmail";
          sudo = "${pkgs.sudo}/bin/sudo";
        in ''
          ${sudo} -u ${config.services.nullmailer.user} ${sendmail} -tf ${cfg.from} <<EOF
          From: ${cfg.from}
          To: ${cfg.to}
          Subject: [$HOST] $UNIT failed

          $(systemctl status -n 1000000 "$UNIT")
          EOF
        '';
      };
    } // (lib.attrsets.genAttrs cfg.services (name: {
      onFailure = [ "notify-failure@%i.service" ];
    }));
  };
}
