{ uint, ... } : {
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
}
