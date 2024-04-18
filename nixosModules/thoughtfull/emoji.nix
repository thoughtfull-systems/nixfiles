{ config, lib, pkgs, ... }: lib.mkIf config.thoughtfull.desktop.enable {
  i18n.inputMethod = {
    enabled = "ibus";
    ibus.engines = with pkgs.ibus-engines; [ uniemoji ];
  };
}
