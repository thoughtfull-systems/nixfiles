{ config, lib, pkgs, ... } : {
  options.thoughtfull.greek.enable = lib.mkEnableOption "greek";
  config = lib.mkIf config.thoughtfull.greek.enable {
    fonts.packages = [
      pkgs.galatia-sil
    ];
    services.xserver = {
      layout = lib.mkOverride 900 "us,gr";
      xkbVariant = lib.mkOverride 900 "dvorak,polytonic";
    };
  };
}
