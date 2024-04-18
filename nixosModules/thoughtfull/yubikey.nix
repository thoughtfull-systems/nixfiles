{ lib, pkgs, ... }: {
  environment.systemPackages = with pkgs; [
    age-plugin-yubikey
    xfce.xfce4-genmon-plugin
    yubikey-manager
    yubikey-manager-qt
  ] ++
  (if pkgs ? yubioath-flutter
   then
     # 23.05+
     [ yubioath-flutter ]
   else
     # 22.11
     [ yubioath-desktop ]);
  hardware.gpgSmartcards.enable = lib.mkDefault true;
  services.pcscd.enable = lib.mkDefault true;
}
