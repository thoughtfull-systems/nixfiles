{ pkgs, ... } : let
  pjs-autostart = pkgs.makeDesktopItem {
    desktopName = "pjstadig User Autostart";
    name = "pjs-autostart";
    exec = "systemctl --user start pjs-autostart.target";
  };
in {
  home.file.pjs-autostart = {
    source = "${pjs-autostart}/share/applications/pjs-autostart.desktop";
    target = ".config/autostart/pjs-autostart.desktop";
  };
  systemd.user.targets.pjs-autostart = {
    Unit = {
      Description = "pjstadig User Autostart";
    };
  };
}
