{ config, lib, pkgs, ... }: lib.mkIf config.thoughtfull.desktop.enable {
  i18n.inputMethod = {
    enabled = lib.mkDefault "ibus";
    ibus.engines = [ pkgs.ibus-engines.uniemoji ];
  };
}
