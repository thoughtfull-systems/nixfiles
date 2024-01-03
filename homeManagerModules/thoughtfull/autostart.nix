{ pkgs, ... } : let
  thoughtfull-autostart = pkgs.makeDesktopItem {
    desktopName = "Thoughtfull Systems User Autostart";
    name = "thoughtfull-autostart";
    exec = "systemctl --user start thoughtfull-autostart.target";
  };
in {
  home.file.thoughtfull-autostart = {
    source = "${thoughtfull-autostart}/share/applications/thoughtfull-autostart.desktop";
    target = ".config/autostart/thoughtfull-autostart.desktop";
  };
  systemd.user.targets.thoughtfull-autostart = {
    Unit = {
      Description = "Thoughtfull Systems User Autostart";
    };
  };
}
