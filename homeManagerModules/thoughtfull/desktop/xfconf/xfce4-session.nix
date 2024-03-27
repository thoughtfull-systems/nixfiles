{ pkgs, ... } : {
  "chooser/AlwaysDisplay" = false;
  "compat/LaunchGNOME" = false;
  "compat/LaunchKDE" = false;
  "general/AutoSave" = false;
  "general/LockCommand" = "${pkgs.xfce.xfce4-screensaver}/bin/xfce4-screensaver-command -l";
  "general/PromptOnLogout" = true;
  "general/SaveOnExit" = false;
  "general/StartAssistiveTechnologies" = false;
  "security/EnableTcp" = false;
  "sessions/Failsafe/Client0_Command" = [ "xfsettingsd" ];
  "sessions/Failsafe/Client0_PerScreen" = false;
  "sessions/Failsafe/Client0_Priority" = 15;
  "sessions/Failsafe/Client1_Command" = [ "${pkgs.thoughtfull.exwm-trampoline}/bin/exwm-trampoline" ];
  "sessions/Failsafe/Client1_PerScreen" = false;
  "sessions/Failsafe/Client1_Priority" = 20;
  "sessions/Failsafe/Client2_Command" = [ "Thunar" "--daemon" ];
  "sessions/Failsafe/Client2_PerScreen" = false;
  "sessions/Failsafe/Client2_Priority" = 25;
  "sessions/Failsafe/Client3_Command" = [ "xfce4-panel" "--disable-wm-check" ];
  "sessions/Failsafe/Client3_PerScreen" = false;
  "sessions/Failsafe/Client3_Priority" = 30;
  "sessions/Failsafe/Count" = 4;
  "shutdown/LockScreen" = false;
}
