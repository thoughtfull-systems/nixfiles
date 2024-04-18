{ pkgs, ... } : {
  systemd.services.lock-screen = {
    before = [
      "hybrid-sleep.target"
      "suspend.target"
      "suspend-then-hibernate.target"
    ];
    enable = true;
    script = "${pkgs.systemd}/bin/loginctl lock-sessions";
    wantedBy = [
      "hybrid-sleep.target"
      "suspend.target"
      "suspend-then-hibernate.target"
    ];
  };
}
