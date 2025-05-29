nixpkgs: let
  notify = nixpkgs.replaceVarsWith {
    dir = "bin";
    isExecutable = true;
    replacements = {
      notify = "${nixpkgs.notify-desktop}/bin/notify-desktop";
      status = "${status}/bin/mic-status";
    };
    src = ./mic-status-notify;
  };
  pactl = "${nixpkgs.pulseaudio}/bin/pactl";
  status = nixpkgs.replaceVarsWith {
    dir = "bin";
    isExecutable = true;
    replacements = {
      inherit pactl;
    };
    src = ./mic-status;
  };
in nixpkgs.symlinkJoin {
  name = "mic";
  paths = [
    (nixpkgs.replaceVarsWith {
      dir = "bin";
      isExecutable = true;
      replacements = {
        inherit pactl;
        notify = "${notify}/bin/mic-status-notify";
      };
      src = ./mic-volume-lower;
    })
    (nixpkgs.replaceVarsWith {
      dir = "bin";
      isExecutable = true;
      replacements = {
        inherit pactl;
        notify = "${notify}/bin/mic-status-notify";
      };
      src = ./mic-mute;
    })
    (nixpkgs.replaceVarsWith {
      dir = "bin";
      isExecutable = true;
      replacements = {
        inherit pactl;
        notify = "${notify}/bin/mic-status-notify";
      };
      src = ./mic-volume-raise;
    })
    notify
    status
  ];
}
