{ config, lib, osConfig, pkgs, ... }: let
  cfg = config.xfconf;
  enable = config.thoughtfull.desktop.enable &&
           osConfig.services.xserver.desktopManager.xfce.enable;
  uint = value: { type = "uint"; value = value; };
in lib.mkIf enable {
  services.picom.enable = true;
  xfconf = {
    settings = {
      pointers = {
        "ELAN067600_04F33195_Touchpad/Properties/libinput_Tapping_Enabled" = 0;
      };
      xfce4-keyboard-shortcuts = {
        "commands/custom/<Alt><Super>s" = null;
        "commands/custom/<Alt>F1" = null;
        "commands/custom/<Alt>F2" = null;
        "commands/custom/<Alt>F2/startup-notify" = null;
        "commands/custom/<Alt>F3" = null;
        "commands/custom/<Alt>F3/startup-notify" = null;
        "commands/custom/<Alt>Print" = null;
        "commands/custom/<Primary><Alt>Delete" = null;
        "commands/custom/<Primary><Alt>Escape" = null;
        "commands/custom/<Primary><Alt>f" = null;
        "commands/custom/<Primary><Alt>l" = null;
        "commands/custom/<Primary><Alt>t" = null;
        "commands/custom/<Primary><Shift>Escape" = null;
        "commands/custom/<Primary>Escape" = null;
        "commands/custom/<Shift>Print" = null;
        "commands/custom/<Super>e" = null;
        "commands/custom/<Super>p" = null;
        "commands/custom/<Super>r" = null;
        "commands/custom/<Super>r/startup-notify" = null;
        "commands/custom/HomePage" = null;
        "commands/custom/Print" = null;
        "commands/custom/XF86Display" = null;
        "commands/custom/XF86Mail" = null;
        "commands/custom/XF86WWW" = null;
      };
      xfce4-notifyd = {
        "do-fadeout" = true;
        "do-slideout" = true;
        "expire-timeout" = 5;
        "expire-timeout-allow-override" = true;
        "expire-timeout-enabled" = true;
        "gauge-ignores-dnd" = true; # show volume changes even with DnD
        "initial-opacity" = 0.8;
        "log-level" = uint 1;
        "log-level-apps" = uint 0;
        "log-max-size" = uint 1000;
        "log-max-size-enabled" = true;
        "notification-display-fields" = "icon-summary-body";
        "notification-log" = true;
        "notify-location" = uint 1; # show bottom left corner
        "plugin/after-menu-shown" = "mark-shown-read";
        "plugin/hide-clear-prompt" = true;
        "plugin/hide-on-read" = false;
        "plugin/log-display-limit" = 10;
        "plugin/log-icon-size" = 16;
        "plugin/log-only-today" = false;
        "plugin/show-in-menu" = "show-all";
        "primary-monitor" = uint 0; # show on monitor with mouse pointer
        "show-text-with-gauge" = true; # show percentage with volume change
        "theme"  = "Default";
      };
      xfce4-panel = {
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
        "plugins/plugin-12/digital-layout" = uint 3; # full date
        "plugins/plugin-12/digital-time-font" = "B612 11";
        "plugins/plugin-12/digital-time-format" = "%R"; # hh:mm
        "plugins/plugin-12/mode" = uint 2; # digital
        "plugins/plugin-12/timezone" = "";
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
        "plugins/plugin-16/base-directory" = "/home/paul";
        "plugins/plugin-16/hidden-files" = false;
        "plugins/plugin-16/new-document" = false;
        "plugins/plugin-16/new-folder" = false;
        "plugins/plugin-16/open-in-terminal" = false;
        "plugins/plugin-17" = "xkb";
        "plugins/plugin-17/display-type" = uint 0; # image
        "plugins/plugin-17/display-name" = uint 0; # country
        "plugins/plugin-17/display-scale" = uint 80;
        "plugins/plugin-17/show-notifications" = false;
        "plugins/plugin-17/display-tooltip-icon" = false;
        "plugins/plugin-17/group-policy" = uint 0; # configure globally
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
      };
      xfce4-power-manager = {
        "xfce4-power-manager/battery-button-action" = uint 0; # nothing
        "xfce4-power-manager/blank-on-ac" = 0; # never
        "xfce4-power-manager/blank-on-battery" = 0; # never
        "xfce4-power-manager/brightness-level-on-ac" = uint 100;
        "xfce4-power-manager/brightness-level-on-battery" = uint 100;
        "xfce4-power-manager/brightness-on-ac" = uint 9; # never
        "xfce4-power-manager/brightness-on-battery" = uint 9; # never
        "xfce4-power-manager/critical-power-action" = uint 2; # hybrid-sleep
        "xfce4-power-manager/critical-power-level" = uint 10;
        "xfce4-power-manager/dpms-enabled" = true; # sleep display after inactivity?
        "xfce4-power-manager/dpms-on-ac-off" = uint 0; # never
        "xfce4-power-manager/dpms-on-ac-sleep" = uint 0; # never
        "xfce4-power-manager/dpms-on-battery-off" = uint 10;
        "xfce4-power-manager/dpms-on-battery-sleep" = uint 10;
        "xfce4-power-manager/general-notification" = false;
        "xfce4-power-manager/handle-brightness-keys" = true;
        "xfce4-power-manager/hibernate-button-action" = uint 2; # hybrid-sleep
        "xfce4-power-manager/inactivity-on-ac" = uint 14; # never
        "xfce4-power-manager/inactivity-on-battery" = uint 20;
        "xfce4-power-manager/inactivity-sleep-mode-on-ac" = uint 1; # suspend
        "xfce4-power-manager/inactivity-sleep-mode-on-battery" = uint 1; # suspend
        "xfce4-power-manager/lock-screen-suspend-hibernate" = true;
        "xfce4-power-manager/logind-handle-lid-switch" = false;
        "xfce4-power-manager/power-button-action" = uint 2; # hybrid-sleep
        "xfce4-power-manager/show-panel-label" = 1; # percentage
        "xfce4-power-manager/show-presentation-indicator" = true;
        "xfce4-power-manager/show-tray-icon" = false;
        "xfce4-power-manager/sleep-button-action" = uint 1; # suspend
      };
      xfce4-screensaver = {
        "lock/embedded-keyboard/enabled" = false;
        "lock/enabled" = true;
        "lock/logout/enabled" = false;
        "lock/saver-activation/enabled" = false;
        "lock/status-messages/enabled" = true;
        "lock/user-switching/enabled" = true;
        "saver/enabled" = false;
        "saver/idle-activation/delay" = 1440;
        "saver/idle-activation/enabled" = false;
      };
      xfce4-session = {
        # starting emacs after xfsettingsd fixes the `cl-no-applicable-method: No applicable method:
        # xcb:-+request, nil, #s(xcb:SetInputFocus t 42 1 nil 0)' error on login
        "sessions/Failsafe/Client0_Command" = [ "xfsettingsd" ];
        "sessions/Failsafe/Client0_PerScreen" = false;
        "sessions/Failsafe/Client0_Priority" = 15;
        "sessions/Failsafe/Client1_Command" = [ "${pkgs.thoughtfull.exwm-trampoline}/bin/exwm-trampoline" ];
        "sessions/Failsafe/Client1_PerScreen" = false;
        "sessions/Failsafe/Client1_Priority" = 20;
        "sessions/Failsafe/Client2_Command" = [ "Thunar" "--daemon" ];
        "sessions/Failsafe/Client2_PerScreen" = false;
        "sessions/Failsafe/Client2_Priority" = 25;
        "sessions/Failsafe/Client3_Command" = [ "xfce4-panel" ];
        "sessions/Failsafe/Client3_PerScreen" = false;
        "sessions/Failsafe/Client3_Priority" = 30;
        "sessions/Failsafe/Count" = 4;
        "general/SaveOnExit" = false;
      };
      xsettings = {
        "Gdk/WindowScalingFactor" = 1;
        "Gtk/ButtonImages" = true;
        "Gtk/CanChangeAccels" = false;
        "Gtk/DialogsUseHeader" = false;
        "Gtk/FontName" = "B612 11";
        "Gtk/MenuImages" = true;
        "Gtk/MonospaceFontName" = "Source Code Pro 11";
        "Net/EnableEventSounds" = false;
        "Net/EnableInputFeedbackSounds" = false;
        "Net/IconThemeName" = "Adwaita";
        "Net/ThemeName" = "Adwaita";
        "Xfce/LastCustomDPI" = 96;
        "Xft/Antialias" = 1;
        "Xft/DPI" = 96;
        "Xft/HintStyle" = "hintfull";
        "Xft/RGBA" = "none";
      };
    };
  };
}
