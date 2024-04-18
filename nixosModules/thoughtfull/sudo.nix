{ lib, pkgs, ... }: {
  security.sudo = {
    execWheelOnly = lib.mkDefault true;
    extraConfig = ''
      Defaults timestamp_type=global,timestamp_timeout=-1
      Defaults!/run/current-system/sw/bin/nixos-rebuild env_keep+=SSH_AUTH_SOCK
    '';
  };
  systemd.services.sudo-reset = {
    description = "Reset sudo timeout upon resume from sleep";
    partOf = [ "post-resume.target" ];
    serviceConfig = {
      ExecStart = "${pkgs.bash}/bin/bash -c 'rm -f /run/sudo/ts/*'";
      RemainAfterExit = "yes";
      Type = "oneshot";
    };
    wantedBy = [ "post-resume.target" ];
  };
}
