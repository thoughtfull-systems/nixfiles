{ config, lib, pkgs, utils, ... }:
lib.mkIf config.services.xserver.desktopManager.xfce.enable {
  nixpkgs.overlays = [
    (self: super: {
      xfce = super.xfce // {
        libxfce4ui = super.xfce.libxfce4ui.overrideAttrs (
          prevAttrs: {
            src = pkgs.fetchFromGitLab {
              domain = "gitlab.xfce.org";
              owner = "xfce";
              repo = "libxfce4ui";
              rev = "libxfce4ui-4.18.4";
              sha256 = "sha256-HnLmZftvFvQAvmQ7jZCaYAQ5GB0YMjzhqZkILzvifoE=";
            };
            version = "4.18.4";
          }
        );
        xfce4-power-manager = super.xfce.xfce4-power-manager.overrideAttrs (
          prevAttrs: {
            buildInputs = (utils.removePackagesByName
              prevAttrs.buildInputs
              [ super.xfce.libxfce4ui ]) ++
            [ self.xfce.libxfce4ui ];
            src = pkgs.fetchFromGitLab {
              domain = "gitlab.xfce.org";
              owner = "pjstadig";
              repo = "xfce4-power-manager";
              rev = "xfce-4.18";
              sha256 = "sha256-nWvyt9nMYU7erHftmEzIGOiNpELlHyyJvg17pA29Rsc=";
            };
            version = "4.18.2";
          });
      };
    })
  ];
  environment = {
    systemPackages = with pkgs.xfce; [
      xfce4-panel
      xfce4-pulseaudio-plugin
      xfce4-xkb-plugin
      xfce4-weather-plugin
    ] ++ [
      pkgs.gnome.file-roller
    ];
    xfce.excludePackages = [ pkgs.xfce.xfce4-volumed-pulse ];
  };
  programs.thunar.plugins = with pkgs.xfce; [
    thunar-archive-plugin
    thunar-media-tags-plugin
    thunar-volman
  ];
  services = {
    xserver = {
      desktopManager.xfce = {
        enableScreensaver = lib.mkDefault false;
        enableXfwm = lib.mkDefault true;
        noDesktop = lib.mkDefault true;
      };
      displayManager.lightdm.enable = true;
      libinput.touchpad.tapping = false;
    };
    logind.lidSwitch = "ignore";
  };
}
