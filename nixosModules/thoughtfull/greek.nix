{ config, lib, pkgs, ... } : let
  greek = config.thoughtfull.greek.enable;
in {
  options.thoughtfull.greek.enable = lib.mkEnableOption "greek";
  config = lib.mkIf greek {
    fonts.packages = [
      pkgs.galatia-sil
    ];
    services.xserver.xkb = {
      layout = lib.mkOverride 900 "us,gr";
      variant = lib.mkOverride 900 "dvorak,polytonic";
    };
  };
}
