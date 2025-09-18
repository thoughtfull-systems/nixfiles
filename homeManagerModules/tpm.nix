{ lib, osConfig, pkgs, ... } : let
  cfg = osConfig.security.tpm2;
  tci = cfg.tciEnvironment;
  iface = tci.interface;
  option = if iface == "tabrmd" then tci.tabrmdConf else tci.deviceConf;
in lib.mkIf cfg.enable
  (let
    fapi-config = (builtins.toJSON {
      profile_name = "P_ECCP256SHA256";
      profile_dir = "${pkgs.tpm2-tss}/etc/tpm2-tss/fapi-profiles/";
      user_dir = "~/.local/share/tpm2-tss/user/keystore/";
      system_dir = "/var/lib/tpm/system/keystore";
      tcti = "${iface}:${option}";
      system_pcrs = [];
      log_dir = "/var/lib/tpm/system/eventlog/";
      firmware_log_file = "/dev/null";
      ima_log_file = "/dev/null";
    });
  in {
    home.environment.TSS2_FAPICONF = fapi-config;
  })
