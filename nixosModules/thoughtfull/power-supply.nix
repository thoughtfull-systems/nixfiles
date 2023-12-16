{ lib, pkgs, writeScript, ... }: let
  script = pkgs.writeScript "dim-screen" ''
    #!/usr/bin/env bash

    case ''${1} in
      ac)
        xbacklight -set 100
        ;;
      bat)
        xbacklight -set 50
        ;;
    esac
  '';
in {
  services.udev = {
    extraRules = ''
      SUBSYSTEM=="power_supply", ATTR{type}=="Mains", ATTR{online}=="0", RUN+="${script} bat"
      SUBSYSTEM=="power_supply", ATTR{type}=="Mains", ATTR{online}=="1", RUN+="${script} ac"
    '';
    path = with pkgs; [
      acpilight
      bash
    ];
  };
}
