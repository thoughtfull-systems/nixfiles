{ pkgs, writeScript, ... }: let
  script = pkgs.writeScript "suspend-when-ac-disconnected" ''
    #!/usr/bin/env bash

    if cat /proc/acpi/button/lid/LID/state | grep "closed"; then
      systemctl suspend
    fi
  '';
in {
  services.udev = {
    extraRules = ''
      SUBSYSTEM=="power_supply", ATTR{type}=="Mains", ATTR{online}=="0", RUN+="${script}"
    '';
  };
}
