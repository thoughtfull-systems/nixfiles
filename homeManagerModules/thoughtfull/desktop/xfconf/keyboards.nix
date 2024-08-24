{ lib, ... } : {
  "Default/KeyRepeat" = lib.mkDefault true;
  "Default/KeyRepeat/Delay" = lib.mkDefault 500;
  "Default/RestoreNumlock" = lib.mkDefault false;
}
