{ config, lib, pkgs, ... }: lib.mkIf config.thoughtfull.desktop.enable {
  i18n.inputMethod = {
    enable = lib.mkDefault true;
    ibus.engines = [ pkgs.ibus-engines.uniemoji ];
    type =  lib.mkDefault "ibus";
  };
}
