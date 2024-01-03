{ lib, pkgs, ... } : {
  systemd.user.services.xss-lock = {
    Install = {
      WantedBy = [ "tfl-autostart.target" ];
    };
    Unit = {
      Description = "XSS Lock Daemon";
      PartOf = [ "tfl-autostart.target" ];
    };
    Service = {
      ExecStart = with lib;
        strings.concatStringsSep " " ([
          "${pkgs.xss-lock}/bin/xss-lock"
          "--session \${XDG_SESSION_ID}"
          "--ignore-sleep"
          "--"
          "${pkgs.lightdm}/bin/dm-tool lock"
        ]);
    };
  };
}
