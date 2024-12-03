{ lib, pkgs, ... }: {
  environment.systemPackages = with pkgs; [
    age-plugin-yubikey
    yubikey-manager
    yubikey-manager-qt
    yubioath-flutter
  ];
  hardware.gpgSmartcards.enable = lib.mkDefault true;
  services.pcscd.enable = lib.mkDefault true;
}
