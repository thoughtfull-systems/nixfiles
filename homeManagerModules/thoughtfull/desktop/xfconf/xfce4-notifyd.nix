{ lib, uint, ... } : {
  "do-fadeout" = lib.mkDefault true;
  "do-slideout" = lib.mkDefault true;
  "expire-timeout" = lib.mkDefault 5;
  "expire-timeout-allow-override" = lib.mkDefault true;
  "expire-timeout-enabled" = lib.mkDefault true;
  "gauge-ignores-dnd" = lib.mkDefault true; # show volume changes even with DnD
  "initial-opacity" = lib.mkDefault 0.8;
  "log-level" = lib.mkDefault (uint 1);
  "log-level-apps" = lib.mkDefault (uint 0);
  "log-max-size" = lib.mkDefault (uint 1000);
  "log-max-size-enabled" = lib.mkDefault true;
  "notification-display-fields" = lib.mkDefault "icon-summary-body";
  "notification-log" = lib.mkDefault true;
  "notify-location" = lib.mkDefault (uint 1); # show bottom left corner
  "plugin/after-menu-shown" = lib.mkDefault "mark-shown-read";
  "plugin/hide-clear-prompt" = lib.mkDefault true;
  "plugin/hide-on-read" = lib.mkDefault false;
  "plugin/log-display-limit" = lib.mkDefault 10;
  "plugin/log-icon-size" = lib.mkDefault 16;
  "plugin/log-only-today" = lib.mkDefault false;
  "plugin/show-in-menu" = lib.mkDefault "show-all";
  "primary-monitor" = lib.mkDefault (uint 0); # show on monitor with mouse pointer
  "show-text-with-gauge" = lib.mkDefault true; # show percentage with volume change
  "theme"  = lib.mkDefault "Default";
}
