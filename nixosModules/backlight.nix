# Dims screen and keyboard backlights when A/C is unplugged.
{ pkgs, ... }: let
  bash = "${pkgs.bash}/bin/bash";
  xbacklight = "${pkgs.acpilight}/bin/xbacklight";
  dim-screen = pkgs.writeScript "dim-screen" ''
    #!${bash}

    case ''${1} in
      ac)
        ${xbacklight} -ctrl intel_backlight -set 100
        ;;
      bat)
        ${xbacklight} -ctrl intel_backlight -set 50
        ;;
    esac
  '';
  set-keyboard = pkgs.writeScript "set-keyboard" ''
    #!${bash}
    bl=$(${xbacklight} -ctrl intel_backlight -get)
    ${xbacklight} -ctrl tpacpi::kbd_backlight -set $((100-$bl))
  '';
in {
  services.udev = {
    extraRules = ''
      SUBSYSTEM=="power_supply", ATTR{type}=="Mains", ATTR{online}=="0", RUN+="${dim-screen} bat"
      SUBSYSTEM=="power_supply", ATTR{type}=="Mains", ATTR{online}=="1", RUN+="${dim-screen} ac"
      SUBSYSTEM=="backlight", ACTION=="change", RUN+="${set-keyboard} ac"
    '';
  };
}
