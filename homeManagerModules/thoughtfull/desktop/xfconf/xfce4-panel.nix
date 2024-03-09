{ config, pkgs, uint, ... } : {
  "configver" = 2;
  "panels" = [ 1 ];
  "panels/dark-mode" = false;
  "panels/panel-1/autohide-behavior" = uint 0; # never
  "panels/panel-1/background-style" = uint 0; # use system style
  "panels/panel-1/enable-struts" = true;
  "panels/panel-1/enter-opacity" = uint 100;
  "panels/panel-1/icon-size" = uint 16;
  "panels/panel-1/leave-opacity" = uint 100;
  "panels/panel-1/length" = 100.0;
  "panels/panel-1/length-adjust" = true;
  "panels/panel-1/mode" = uint 0; # horizontal
  "panels/panel-1/nrows" = uint 1;
  "panels/panel-1/plugin-ids" = [ 1 16 3 20 6 17 8 10 9 19 12 18 15 14 ];
  "panels/panel-1/position" = "p=6;x=0;y=0";
  "panels/panel-1/position-locked" = true;
  "panels/panel-1/size" = uint 26;
  "plugins/plugin-1" = "applicationsmenu";
  "plugins/plugin-1/button-icon" = "org.xfce.panel.applicationsmenu";
  "plugins/plugin-1/button-title" =  "Applications";
  "plugins/plugin-1/custom-menu" = false;
  "plugins/plugin-1/show-button-title" = false;
  "plugins/plugin-1/show-generic-names" = false;
  "plugins/plugin-1/show-menu-icons" = true;
  "plugins/plugin-1/show-tooltips" = false;
  "plugins/plugin-1/small" = true; # show on one line
  "plugins/plugin-3" = "separator";
  "plugins/plugin-3/expand" = true;
  "plugins/plugin-3/style" = uint 0; # transparent
  "plugins/plugin-6" = "systray";
  "plugins/plugin-6/hide-new-items" = false;
  "plugins/plugin-6/icon-size" = 16;
  "plugins/plugin-6/known-legacy-items" = [
    "ethernet network connection \"wired connection 1\" active"
    "networkmanager applet"
    "xfce4-power-manager"
  ];
  "plugins/plugin-6/menu-is-primary" = false; # must right click for menu
  "plugins/plugin-6/single-row" = false;
  "plugins/plugin-6/square-icons" = true;
  "plugins/plugin-6/symbolic-icons" = false;
  "plugins/plugin-8" = "pulseaudio";
  "plugins/plugin-8/enable-keyboard-shortcuts" = true; # volume keys
  "plugins/plugin-8/enable-mpris" = true; # control media players
  "plugins/plugin-8/enable-multimedia-keys" = true; # media player keys
  "plugins/plugin-8/mixer-command" =  "pavucontrol";
  "plugins/plugin-8/play-sound" = true; # play volume adjustment sound
  "plugins/plugin-8/show-notifications" = uint 1; # all
  "plugins/plugin-8/volume-step" = uint 5;
  "plugins/plugin-9" = "power-manager-plugin";
  "plugins/plugin-10" = "notification-plugin";
  "plugins/plugin-12" = "clock";
  "plugins/plugin-12/command" = "";
  "plugins/plugin-12/digital-date-font" = "B612 8";
  "plugins/plugin-12/digital-layout" = uint 1; # time then date
  "plugins/plugin-12/digital-time-font" = "B612 Bold 10";
  "plugins/plugin-12/digital-time-format" = "%R"; # hh:mm
  "plugins/plugin-12/mode" = uint 2; # digital
  "plugins/plugin-12/timezone" = "";
  "plugins/plugin-12/tooltip-format" = "Week %V";
  "plugins/plugin-14" = "actions";
  "plugins/plugin-14/appearance" = uint 0; # action buttons
  "plugins/plugin-14/ask-confirmation" = false;
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
  "plugins/plugin-15" = "separator";
  "plugins/plugin-15/expand" = false;
  "plugins/plugin-15/style" = uint 1; # separator
  "plugins/plugin-16" = "directorymenu";
  "plugins/plugin-16/base-directory" = config.home.homeDirectory;
  "plugins/plugin-16/hidden-files" = false;
  "plugins/plugin-16/new-document" = false;
  "plugins/plugin-16/new-folder" = false;
  "plugins/plugin-16/open-in-terminal" = false;
  "plugins/plugin-17" = "xkb";
  "plugins/plugin-17/display-name" = uint 1; # language
  "plugins/plugin-17/display-scale" = uint 80;
  "plugins/plugin-17/display-tooltip-icon" = true;
  "plugins/plugin-17/display-type" = uint 2; # system
  "plugins/plugin-17/group-policy" = uint 0; # globally
  "plugins/plugin-17/show-notifications" = false;
  "plugins/plugin-18" = "weather";
  "plugins/plugin-18/forecast/days" = 5;
  "plugins/plugin-18/forecast/layout" = 0; # columns
  "plugins/plugin-18/labels/label0" = 3; # temperature
  "plugins/plugin-18/power-saving" = true;
  "plugins/plugin-18/round" = true;
  "plugins/plugin-18/scrollbox/animate" = true;
  "plugins/plugin-18/scrollbox/lines" = 1;
  "plugins/plugin-18/scrollbox/show" = true;
  "plugins/plugin-18/scrollbox/use-color" = false;
  "plugins/plugin-18/single-row" = true;
  "plugins/plugin-18/tooltip-style" = 0; # simple
  "plugins/plugin-18/units/altitude" = 1; # feet
  "plugins/plugin-18/units/apparent-temperature" = 0; # windchill/heat index
  "plugins/plugin-18/units/precipitation" = 1; # inches
  "plugins/plugin-18/units/pressure" = 2; # psi
  "plugins/plugin-18/units/temperature" = 1; # fahrenheit
  "plugins/plugin-18/units/windspeed" = 1; # mph
  "plugins/plugin-19" = "separator";
  "plugins/plugin-19/expand" = false;
  "plugins/plugin-19/style" = uint 1; # separator
  "plugins/plugin-20" = "genmon";
  "plugins/plugin-20/command" = "${pkgs.thoughtfull.yubikey-touch-plugin}/bin/yubikey-touch-plugin";
  "plugins/plugin-20/enable-single-row" = true;
  "plugins/plugin-20/font" = "B612 11";
  "plugins/plugin-20/text" = "";
  "plugins/plugin-20/update-period" = 250;
  "plugins/plugin-20/use-label" = false;
}
