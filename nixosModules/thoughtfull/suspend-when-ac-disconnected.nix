{ config, lib, pkgs, ... }: let
  desktop = config.thoughtfull.desktop.enable;
  bash = "${pkgs.bash}/bin/bash";
  script = pkgs.writeScript "suspend-when-ac-disconnected" ''
    #!${bash}

    if cat /proc/acpi/button/lid/LID/state | grep "closed"; then
      systemctl suspend
    fi
  '';
in {
  services.udev.extraRules = lib.mkIf desktop ''
    SUBSYSTEM=="power_supply", ATTR{type}=="Mains", ATTR{online}=="0", RUN+="${script}"
  '';
}
