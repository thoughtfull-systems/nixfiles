{ config, lib, pkgs, ... }: lib.mkIf config.thoughtfull.desktop.enable {
  fonts = {
    enableDefaultPackages = lib.mkDefault true;
    fontconfig.enable = lib.mkDefault true;
    packages = [ pkgs.corefonts ];
  };
}
