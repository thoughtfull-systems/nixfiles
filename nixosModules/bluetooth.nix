{ config, lib, ... }: {
  services.blueman.enable = lib.mkDefault config.hardware.bluetooth.enable;
}
