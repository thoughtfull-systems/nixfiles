{ config, lib, pkgs, ... } : {
  options.thoughtfull.greek.enable = lib.mkEnableOption "desktop";
  config = lib.mkIf config.thoughtfull.greek.enable {
    fonts.fonts = [
      pkgs.galatia-sil
    ];
    services.xserver = {
      layout = lib.mkOverride 900 "dvorak,us,gr";
      xkbVariant = lib.mkOverride 900 ",,polytonic";
    };
  };
}
