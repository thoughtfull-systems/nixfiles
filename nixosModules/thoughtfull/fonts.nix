{ config, lib, pkgs, ... }: lib.mkIf config.thoughtfull.desktop.enable {
  fonts = {
    enableDefaultPackages = true;
    fontconfig.enable = true;
    packages = [ pkgs.corefonts ];
  };
}
