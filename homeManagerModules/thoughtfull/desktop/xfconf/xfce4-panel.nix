{ config, lib, pkgs, uint, ... } : let
  yubikey-touch-plugin = "${pkgs.thoughtfull.yubikey-touch-plugin}/bin/yubikey-touch-plugin";
in {
  "configver" = lib.mkDefault 2;
  "panels" = [ 1 ];
  "panels/dark-mode" = lib.mkDefault false;
  "panels/panel-1/autohide-behavior" = lib.mkDefault (uint 0); # never
  "panels/panel-1/background-style" = lib.mkDefault (uint 0); # use system style
  "panels/panel-1/enable-struts" = lib.mkDefault true;
  "panels/panel-1/enter-opacity" = lib.mkDefault (uint 100);
  "panels/panel-1/icon-size" = lib.mkDefault (uint 16);
  "panels/panel-1/leave-opacity" = lib.mkDefault (uint 100);
  "panels/panel-1/length" = lib.mkDefault 100.0;
  "panels/panel-1/length-adjust" = lib.mkDefault true;
  "panels/panel-1/mode" = lib.mkDefault (uint 0); # horizontal
  "panels/panel-1/nrows" = lib.mkDefault (uint 1);
  "panels/panel-1/plugin-ids" = [ 1 16 3 20 6 17 8 10 9 19 12 21 18 15 14 ];
  "panels/panel-1/position" = lib.mkDefault "p=6;x=0;y=0";
  "panels/panel-1/position-locked" = lib.mkDefault true;
  "panels/panel-1/size" = lib.mkDefault (uint 26);
  "plugins/plugin-1" = lib.mkDefault "applicationsmenu";
  "plugins/plugin-1/button-icon" = lib.mkDefault "org.xfce.panel.applicationsmenu";
  "plugins/plugin-1/button-title" = lib.mkDefault  "Applications";
  "plugins/plugin-1/custom-menu" = lib.mkDefault false;
  "plugins/plugin-1/show-button-title" = lib.mkDefault false;
  "plugins/plugin-1/show-generic-names" = lib.mkDefault false;
  "plugins/plugin-1/show-menu-icons" = lib.mkDefault true;
  "plugins/plugin-1/show-tooltips" = lib.mkDefault false;
  "plugins/plugin-1/small" = lib.mkDefault true; # show on one line
  "plugins/plugin-3" = lib.mkDefault "separator";
  "plugins/plugin-3/expand" = lib.mkDefault true;
  "plugins/plugin-3/style" = lib.mkDefault (uint 0); # transparent
  "plugins/plugin-6" = lib.mkDefault "systray";
  "plugins/plugin-6/hide-new-items" = lib.mkDefault false;
  "plugins/plugin-6/icon-size" = lib.mkDefault 16;
  "plugins/plugin-6/known-legacy-items" = [
    "ethernet network connection \"wired connection 1\" active"
    "networkmanager applet"
    "xfce4-power-manager"
  ];
  "plugins/plugin-6/menu-is-primary" = lib.mkDefault false; # must right click for menu
  "plugins/plugin-6/single-row" = lib.mkDefault false;
  "plugins/plugin-6/square-icons" = lib.mkDefault true;
  "plugins/plugin-6/symbolic-icons" = lib.mkDefault false;
  "plugins/plugin-8" = lib.mkDefault "pulseaudio";
  "plugins/plugin-8/enable-keyboard-shortcuts" = lib.mkDefault true; # volume keys
  "plugins/plugin-8/enable-mpris" = lib.mkDefault true; # control media players
  "plugins/plugin-8/enable-multimedia-keys" = lib.mkDefault true; # media player keys
  "plugins/plugin-8/mixer-command" = lib.mkDefault  "pavucontrol";
  "plugins/plugin-8/play-sound" = lib.mkDefault true; # play volume adjustment sound
  "plugins/plugin-8/show-notifications" = lib.mkDefault (uint 1); # all
  "plugins/plugin-8/volume-step" = lib.mkDefault (uint 5);
  "plugins/plugin-9" = lib.mkDefault "power-manager-plugin";
  "plugins/plugin-10" = lib.mkDefault "notification-plugin";
  "plugins/plugin-12" = lib.mkDefault "clock";
  "plugins/plugin-12/command" = lib.mkDefault "";
  "plugins/plugin-12/digital-time-font" = lib.mkDefault "B612 11";
  "plugins/plugin-12/digital-time-format" = lib.mkDefault "%H:%M";
  "plugins/plugin-12/digital-layout" = lib.mkDefault (uint 3); # time only
  "plugins/plugin-12/mode" = lib.mkDefault (uint 2); # digital
  "plugins/plugin-12/timezone" = lib.mkDefault "";
  "plugins/plugin-12/tooltip-format" = lib.mkDefault "%A, %B %d, %Y (Week %V)";
  "plugins/plugin-14" = lib.mkDefault "actions";
  "plugins/plugin-14/appearance" = lib.mkDefault (uint 0); # action buttons
  "plugins/plugin-14/ask-confirmation" = lib.mkDefault false;
  "plugins/plugin-14/items" = [
    "+suspend"
    "+hybrid-sleep"
    "+logout"
    "-separator"
    "-switch-user"
    "-hibernate"
    "-shutdown"
    "-restart"
    "-logout-dialog"
    "-lock-screen"
  ];
  "plugins/plugin-15" = lib.mkDefault "separator";
  "plugins/plugin-15/expand" = lib.mkDefault false;
  "plugins/plugin-15/style" = lib.mkDefault (uint 1); # separator
  "plugins/plugin-16" = lib.mkDefault "directorymenu";
  "plugins/plugin-16/base-directory" = lib.mkDefault config.home.homeDirectory;
  "plugins/plugin-16/hidden-files" = lib.mkDefault false;
  "plugins/plugin-16/new-document" = lib.mkDefault false;
  "plugins/plugin-16/new-folder" = lib.mkDefault false;
  "plugins/plugin-16/open-in-terminal" = lib.mkDefault false;
  "plugins/plugin-17" = lib.mkDefault "xkb";
  "plugins/plugin-17/display-name" = lib.mkDefault (uint 1); # language
  "plugins/plugin-17/display-scale" = lib.mkDefault (uint 80);
  "plugins/plugin-17/display-tooltip-icon" = lib.mkDefault true;
  "plugins/plugin-17/display-type" = lib.mkDefault (uint 2); # system
  "plugins/plugin-17/group-policy" = lib.mkDefault (uint 0); # globally
  "plugins/plugin-17/show-notifications" = lib.mkDefault false;
  "plugins/plugin-18" = lib.mkDefault "weather";
  "plugins/plugin-18/forecast/days" = lib.mkDefault 5;
  "plugins/plugin-18/forecast/layout" = lib.mkDefault 0; # columns
  "plugins/plugin-18/labels/label0" = lib.mkDefault 3; # temperature
  "plugins/plugin-18/power-saving" = lib.mkDefault true;
  "plugins/plugin-18/round" = lib.mkDefault true;
  "plugins/plugin-18/scrollbox/animate" = lib.mkDefault true;
  "plugins/plugin-18/scrollbox/lines" = lib.mkDefault 1;
  "plugins/plugin-18/scrollbox/show" = lib.mkDefault true;
  "plugins/plugin-18/scrollbox/use-color" = lib.mkDefault false;
  "plugins/plugin-18/single-row" = lib.mkDefault true;
  "plugins/plugin-18/tooltip-style" = lib.mkDefault 0; # simple
  "plugins/plugin-18/units/altitude" = lib.mkDefault 1; # feet
  "plugins/plugin-18/units/apparent-temperature" = lib.mkDefault 0; # windchill/heat index
  "plugins/plugin-18/units/precipitation" = lib.mkDefault 1; # inches
  "plugins/plugin-18/units/pressure" = lib.mkDefault 2; # psi
  "plugins/plugin-18/units/temperature" = lib.mkDefault 1; # fahrenheit
  "plugins/plugin-18/units/windspeed" = lib.mkDefault 1; # mph
  "plugins/plugin-19" = lib.mkDefault "separator";
  "plugins/plugin-19/expand" = lib.mkDefault false;
  "plugins/plugin-19/style" = lib.mkDefault (uint 1); # separator
  "plugins/plugin-20" = lib.mkDefault "genmon";
  "plugins/plugin-20/command" = lib.mkDefault yubikey-touch-plugin;
  "plugins/plugin-20/enable-single-row" = lib.mkDefault true;
  "plugins/plugin-20/font" = lib.mkDefault "B612 11";
  "plugins/plugin-20/text" = lib.mkDefault "";
  "plugins/plugin-20/update-period" = lib.mkDefault 250;
  "plugins/plugin-20/use-label" = lib.mkDefault false;
  "plugins/plugin-21" = lib.mkDefault "genmon";
  "plugins/plugin-21/command" = lib.mkDefault "date -u +'(%R)'";
  "plugins/plugin-21/enable-single-row" = lib.mkDefault true;
  "plugins/plugin-21/font" = lib.mkDefault "B612 Italic 10";
  "plugins/plugin-21/text" = lib.mkDefault "";
  "plugins/plugin-21/update-period" = lib.mkDefault 500;
  "plugins/plugin-21/use-label" = lib.mkDefault false;
}
