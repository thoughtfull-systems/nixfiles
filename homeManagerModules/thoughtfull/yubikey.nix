{ pkgs, ... }: with pkgs; {
  systemd.user = {
    services = {
      # The upstream service uses '--libnotify', which I don't want.
      yubikey-touch-detector = {
        Install = {
          Also = "yubikey-touch-detector.socket";
          WantedBy = [ "graphical-session.target" ];
        };
        Service.ExecStart = "${yubikey-touch-detector}/bin/yubikey-touch-detector";
        Unit = {
          Description = "Detects when your YubiKey is waiting for a touch";
          Requires = [ "yubikey-touch-detector.socket" ];
        };
      };
      yubikey-touch-plugin-updater = {
        Install.WantedBy = [ "graphical-session.target" ];
        Service = {
          Environment = "PATH=${coreutils}/bin:${netcat}/bin:${bash}/bin:${imagemagick}/bin:${notify-desktop}/bin";
          ExecStart = "${thoughtfull.yubikey-touch-plugin}/bin/yubikey-touch-plugin-updater";
        };
      };
    };
    sockets.yubikey-touch-detector = {
      Install.WantedBy = [ "sockets.target" ];
      Socket = {
        ListenStream = "%t/yubikey-touch-detector.socket";
        RemoveOnStop = true;
      };
      Unit.Description = "Unix socket activation for YubiKey touch detector service";
    };
  };
}
