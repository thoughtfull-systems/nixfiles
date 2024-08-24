{ lib, pkgs, ... } : let
  exwm-trampoline = "${pkgs.thoughtfull.exwm-trampoline}/bin/exwm-trampoline";
  xfce4-panel = "${pkgs.xfce.xfce4-panel}/bin/xfce4-panel";
  xfce4-screensaver-command = "${pkgs.xfce.xfce4-screensaver}/bin/xfce4-screensaver-command";
  xfsettingsd = "${pkgs.xfce.xfce4-settings}/bin/xfsettingsd";
in {
  "chooser/AlwaysDisplay" = lib.mkDefault false;
  "compat/LaunchGNOME" = lib.mkDefault false;
  "compat/LaunchKDE" = lib.mkDefault false;
  "general/AutoSave" = lib.mkDefault false;
  "general/LockCommand" = lib.mkDefault "${xfce4-screensaver-command} -l";
  "general/PromptOnLogout" = lib.mkDefault true;
  "general/SaveOnExit" = lib.mkDefault false;
  "general/StartAssistiveTechnologies" = lib.mkDefault false;
  "security/EnableTcp" = lib.mkDefault false;
  "sessions/Failsafe/Client0_Command" = [ xfsettingsd ];
  "sessions/Failsafe/Client0_PerScreen" = lib.mkDefault false;
  "sessions/Failsafe/Client0_Priority" = lib.mkDefault 15;
  "sessions/Failsafe/Client1_Command" = [ exwm-trampoline ];
  "sessions/Failsafe/Client1_PerScreen" = lib.mkDefault false;
  "sessions/Failsafe/Client1_Priority" = lib.mkDefault 20;
  "sessions/Failsafe/Client2_Command" = [ "thunar" "--daemon" ];
  "sessions/Failsafe/Client2_PerScreen" = lib.mkDefault false;
  "sessions/Failsafe/Client2_Priority" = lib.mkDefault 25;
  "sessions/Failsafe/Client3_Command" = [ xfce4-panel "--disable-wm-check" ];
  "sessions/Failsafe/Client3_PerScreen" = lib.mkDefault false;
  "sessions/Failsafe/Client3_Priority" = lib.mkDefault 30;
  "sessions/Failsafe/Count" = lib.mkDefault 4;
  "shutdown/LockScreen" = lib.mkDefault false;
}
