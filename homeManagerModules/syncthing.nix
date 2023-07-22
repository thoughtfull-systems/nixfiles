{ config, lib, ... } : let
  cfg = config.thoughtfull.services.syncthing-init;
in {
  home.file = lib.mkMerge [
    (lib.mkIf cfg.folders.obsidian.enable {
      "obsidian/.stignore".text = ''
        .obsidian/workspace
        .obsidian/workspace.json
        .obsidian/workspace-mobile.json
        Work
      '';
    })
    (lib.mkIf cfg.folders.org.enable {
      "org/.stignore".text = ''
        work
      '';
    })
  ];
  thoughtfull.services.syncthing-init = {
    devices = {
      bennu.id = "ILSWF2W-E6ARU5W-SCYVCOF-5ZE3ZCY-XFVQEA3-M5B2YZP-XEBD7DM-MIKWPAB";
      carbon.id = "W5I2DUS-UWWEX23-WU3LLH5-TVFIW7A-22HMHA6-YDIA54S-XK3BYKC-6DKA2AB";
      pixel.id = "RNY2VPR-2F632JU-NPX3CYQ-Z24UVMT-7ICVGBB-D44M545-UM4T0PD-L4XRVQV";
      pixel5a.id = "42YB5VC-NXUGRVT-ZAA5CAC-BKNVVQS-RVMJ7RY-OBLW6WQ-DCNFNVG-T3XZZAH";
      raspi3b.id = "WBSX6GK-AR57KIM-FKJ42SX-X7WJ33Z-4NNRV4R-GVL7AYN-MF423TN-F3PZMQG";
      hemera.id = "ONFK7RC-44LOUDW-G32XTJA-LXU2HPI-YQH2Y2A-3N2KYCI-GMPODU5-SVIUTQZ";
      ziph.id =   "363D5PB-LZCUQ4H-RCUP5GD-2P43V04-54G53N7-ASELU5K-LALRGTP-RLH7NAW";
    };
    folders = {
      archive = {
        enable = lib.mkDefault false;
        id = "mum9q-7wogj";
        ignorePerms = false;
        path = "~/archive";
        type = "sendreceive";
        versioning = {
          params = {
            maxAge = "31536000";
          };
          type = "staggered";
        };
      };
      obsidian = {
        enable = lib.mkDefault false;
        id = "9sjcz-pgqz3";
        ignorePerms = false;
        path = "~/obsidian";
        type = "sendreceive";
        versioning = {
          params = {
            maxAge = "31536000";
          };
          type = "staggered";
        };
      };
      obsidian-work = {
        enable = lib.mkDefault false;
        id = "uwwsn-76zqc";
        ignorePerms = false;
        path = lib.mkDefault "~/obsidian/Work";
        type = "sendreceive";
        versioning = {
          params = {
            maxAge = "31536000";
          };
          type = "staggered";
        };
      };
      org = {
        enable = lib.mkDefault false;
        id = "7phjt-m2zve";
        ignorePerms = false;
        path = "~/org";
        type = "sendreceive";
        versioning = {
          params = {
            maxAge = "31536000";
          };
          type = "staggered";
        };
      };
      org-work = {
        enable = lib.mkDefault false;
        id = "44lzd-pmrfu";
        ignorePerms = false;
        path = lib.mkDefault "~/org/work";
        type = "sendreceive";
        versioning = {
          params = {
            maxAge = "31536000";
          };
          type = "staggered";
        };
      };
      sync = {
        enable = lib.mkDefault false;
        id = "j5kv6-kpa7s";
        ignorePerms = false;
        path = "~/sync";
        type = "sendreceive";
        versioning = {
          params = {
            maxAge = "31536000";
          };
          type = "staggered";
        };
      };
    };
  };
}
