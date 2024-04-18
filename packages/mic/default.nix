nixpkgs: let
  notify = nixpkgs.substituteAll {
    dir = "bin";
    isExecutable = true;
    src = ./mic-status-notify;
    notify = "${nixpkgs.notify-desktop}/bin/notify-desktop";
    status = "${status}/bin/mic-status";
  };
  pactl = "${nixpkgs.pulseaudio}/bin/pactl";
  status = nixpkgs.substituteAll {
    dir = "bin";
    inherit pactl;
    isExecutable = true;
    src = ./mic-status;
  };
in nixpkgs.symlinkJoin {
  name = "mic";
  paths = [
    (nixpkgs.substituteAll {
      dir = "bin";
      isExecutable = true;
      src = ./mic-volume-lower;
      inherit pactl;
      notify = "${notify}/bin/mic-status-notify";
    })
    (nixpkgs.substituteAll {
      dir = "bin";
      isExecutable = true;
      src = ./mic-mute;
      inherit pactl;
      notify = "${notify}/bin/mic-status-notify";
    })
    (nixpkgs.substituteAll {
      dir = "bin";
      isExecutable = true;
      src = ./mic-volume-raise;
      inherit pactl;
      notify = "${notify}/bin/mic-status-notify";
    })
    notify
    status
  ];
}
