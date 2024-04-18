{ config, lib, pkgs, ... }: {
  options.thoughtfull.notifications.enable = lib.mkEnableOption "notifications";
  config = lib.mkIf config.thoughtfull.notifications.enable {
    home.packages = with pkgs; [
      notify-desktop
    ];
    xsession = {
      enable = true;
      # I examined available notification daemons and found this to be easy to
      # setup and it supports actions.  I'd prefer something gtk based, but I'm
      # not that picky at this point.  I can re-evaluate this later.
      initExtra = "${pkgs.lxqt.lxqt-notificationd}/bin/lxqt-notificationd &";
    };
  };
}
