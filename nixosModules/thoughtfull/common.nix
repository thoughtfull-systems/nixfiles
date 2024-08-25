{ lib, ... }: {
  boot.tmp.cleanOnBoot = lib.mkDefault true;
  hardware.enableAllFirmware = lib.mkDefault true;
  i18n.defaultLocale = lib.mkDefault "en_US.UTF-8";
  networking.domain = lib.mkDefault "thoughtfull.systems";
}
