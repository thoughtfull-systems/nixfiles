{ config, lib, pkgs, ... } : let
  cfg = config.thoughtfull.greek;
in {
  options.thoughtfull.greek.enable = lib.mkEnableOption "greek";
  config = lib.mkIf cfg.enable {
    fonts.packages = [
      pkgs.galatia-sil
    ];
    services.xserver.xkb = {
      layout = lib.mkOverride 900 "us,gr";
      variant = lib.mkOverride 900 "dvorak,polytonic";
    };
  };
}
