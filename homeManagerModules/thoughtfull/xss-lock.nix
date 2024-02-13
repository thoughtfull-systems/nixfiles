{ lib, pkgs, ... } : {
  systemd.user.services.xss-lock = {
    Install = {
      WantedBy = [ "thoughtfull-autostart.target" ];
    };
    Unit = {
      Description = "XSS Lock Daemon";
      PartOf = [ "thoughtfull-autostart.target" ];
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
