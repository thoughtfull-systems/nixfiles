{ lib, osConfig, pkgs, ... } : let
  cfg = osConfig.security.tpm2;
  tcti = cfg.tctiEnvironment;
  iface = tcti.interface;
  option = if iface == "tabrmd" then tcti.tabrmdConf else tcti.deviceConf;
in lib.mkIf cfg.enable
  (let
    fapi-config = (pkgs.writeText "fapi-config.json"
      (builtins.toJSON {
        profile_name = "P_ECCP256SHA256";
        profile_dir = "${pkgs.tpm2-tss}/etc/tpm2-tss/fapi-profiles/";
        user_dir = "~/.local/share/tpm2-tss/user/keystore/";
        system_dir = "/var/lib/tpm/system/keystore";
        tcti = "${iface}:${option}";
        system_pcrs = [];
        log_dir = "/var/lib/tpm/system/eventlog/";
        firmware_log_file = "/dev/null";
        ima_log_file = "/dev/null";
      }));
  in {
    home.sessionVariables.TSS2_FAPICONF = fapi-config;
  })
