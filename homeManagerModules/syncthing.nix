{ config, lib, pkgs, ... }:

with lib;

let
  cfg = config.thoughtfull.services.syncthing-init;

  devices = mapAttrsToList (name: device: {
    deviceID = device.id;
    inherit (device) name addresses introducer autoAcceptFolders;
  }) cfg.devices;

  folders = mapAttrsToList ( _: folder: {
    inherit (folder) path id label type;
    devices = map (device: { deviceId = cfg.devices.${device}.id; }) folder.devices;
    rescanIntervalS = folder.rescanInterval;
    fsWatcherEnabled = folder.watch;
    fsWatcherDelayS = folder.watchDelay;
    ignorePerms = folder.ignorePerms;
    ignoreDelete = folder.ignoreDelete;
    versioning = folder.versioning;
  }) (filterAttrs (
    _: folder:
    folder.enable
  ) cfg.folders);

  enabled = config.services.syncthing.enable;

  updateConfig = pkgs.writers.writeDash "merge-syncthing-config" ''
    set -efu
    # be careful not to leak secrets in the filesystem or in process listings
    umask 0077
    # get the api key by parsing the config.xml
    while
        ! ${pkgs.libxml2}/bin/xmllint \
            --xpath 'string(configuration/gui/apikey)' \
            ${cfg.configDir}/config.xml \
            >"$RUNTIME_DIRECTORY/api_key"
    do sleep 1; done
    (printf "X-API-Key: "; ${pkgs.coreutils-full}/bin/cat "$RUNTIME_DIRECTORY/api_key") >"$RUNTIME_DIRECTORY/headers"
    curl() {
        ${pkgs.curl}/bin/curl -sSLk -H "@$RUNTIME_DIRECTORY/headers" \
            --retry 1000 --retry-delay 1 --retry-all-errors \
            "$@"
    }
    # query the old config
    old_cfg=$(curl ${cfg.guiAddress}/rest/config)
    # generate the new config by merging with the NixOS config options
    new_cfg=$(printf '%s\n' "$old_cfg" | ${pkgs.jq}/bin/jq -c '. * {
        "devices": (${builtins.toJSON devices}${optionalString (cfg.devices == {}) " + .devices"}),
        "folders": (${builtins.toJSON folders}${optionalString (cfg.folders == {}) " + .folders"})
        }')
    # send the new config
    curl -X PUT -d "$new_cfg" ${cfg.guiAddress}/rest/config
    # restart Syncthing if required
    if curl ${cfg.guiAddress}/rest/config/restart-required |
       ${pkgs.jq}/bin/jq -e .requiresRestart > /dev/null; then
        curl -X POST ${cfg.guiAddress}/rest/system/restart
    fi
  '';
in {
  ###### interface
  options.thoughtfull.services.syncthing-init = {
    configDir = mkOption {
      default = "~/.config/syncthing";
      description = mdDoc ''
        Configuration dir for syncthing.
      '';
      type = types.str;
    };

    devices = mkOption {
      default = {};
      description = mdDoc ''
        Peers/devices which Syncthing should communicate with.
        Note that you can still add devices manually, but those changes
        will be reverted on restart if [overrideDevices](#opt-services.syncthing.overrideDevices)
        is enabled.
      '';
      example = {
        bigbox = {
          id = "7CFNTQM-IMTJBHJ-3UWRDIU-ZGQJFR6-VCXZ3NB-XUH3KZO-N52ITXR-LAIYUAU";
          addresses = [ "tcp://192.168.0.10:51820" ];
        };
      };
      type = types.attrsOf (types.submodule ({ name, ... }: {
        options = {

          name = mkOption {
            type = types.str;
            default = name;
            description = lib.mdDoc ''
              The name of the device.
            '';
          };

          addresses = mkOption {
            type = types.listOf types.str;
            default = [];
            description = lib.mdDoc ''
              The addresses used to connect to the device.
              If this is left empty, dynamic configuration is attempted.
            '';
          };

          id = mkOption {
            type = types.str;
            description = mdDoc ''
              The device ID. See <https://docs.syncthing.net/dev/device-ids.html>.
            '';
          };

          introducer = mkOption {
            type = types.bool;
            default = false;
            description = mdDoc ''
              Whether the device should act as an introducer and be allowed
              to add folders on this computer.
              See <https://docs.syncthing.net/users/introducer.html>.
            '';
          };

          autoAcceptFolders = mkOption {
            type = types.bool;
            default = false;
            description = mdDoc ''
              Automatically create or share folders that this device advertises at the default path.
              See <https://docs.syncthing.net/users/config.html?highlight=autoaccept#config-file-format>.
            '';
          };

        };
      }));
    };

    folders = mkOption {
      default = {};
      description = mdDoc ''
        Folders which should be shared by Syncthing.
        Note that you can still add folders manually, but those changes
        will be reverted on restart if [overrideFolders](#opt-services.syncthing.overrideFolders)
        is enabled.
      '';
      example = literalExpression ''
        {
          "/home/user/sync" = {
            id = "syncme";
            devices = [ "bigbox" ];
          };
        }
      '';
      type = types.attrsOf (types.submodule ({ name, ... }: {
        options = {

          enable = mkOption {
            type = types.bool;
            default = true;
            description = lib.mdDoc ''
              Whether to share this folder.
              This option is useful when you want to define all folders
              in one place, but not every machine should share all folders.
            '';
          };

          path = mkOption {
            type = types.str // {
              check = x: types.str.check x && (substring 0 1 x == "/" || substring 0 2 x == "~/");
              description = types.str.description + " starting with / or ~/";
            };
            default = name;
            description = lib.mdDoc ''
              The path to the folder which should be shared.
              Only absolute paths (starting with `/`) and paths relative to
              the [user](#opt-services.syncthing.user)'s home directory
              (starting with `~/`) are allowed.
            '';
          };

          id = mkOption {
            type = types.str;
            default = name;
            description = lib.mdDoc ''
              The ID of the folder. Must be the same on all devices.
            '';
          };

          label = mkOption {
            type = types.str;
            default = name;
            description = lib.mdDoc ''
              The label of the folder.
            '';
          };

          devices = mkOption {
            type = types.listOf types.str;
            default = [];
            description = mdDoc ''
              The devices this folder should be shared with. Each device must
              be defined in the [devices](#opt-services.syncthing.devices) option.
            '';
          };

          versioning = mkOption {
            default = null;
            description = mdDoc ''
              How to keep changed/deleted files with Syncthing.
              There are 4 different types of versioning with different parameters.
              See <https://docs.syncthing.net/users/versioning.html>.
            '';
            example = literalExpression ''
              [
                {
                  versioning = {
                    type = "simple";
                    params.keep = "10";
                  };
                }
                {
                  versioning = {
                    type = "trashcan";
                    params.cleanoutDays = "1000";
                  };
                }
                {
                  versioning = {
                    type = "staggered";
                    fsPath = "/syncthing/backup";
                    params = {
                      cleanInterval = "3600";
                      maxAge = "31536000";
                    };
                  };
                }
                {
                  versioning = {
                    type = "external";
                    params.versionsPath = pkgs.writers.writeBash "backup" '''
                      folderpath="$1"
                      filepath="$2"
                      rm -rf "$folderpath/$filepath"
                    ''';
                  };
                }
              ]
            '';
            type = with types; nullOr (submodule {
              options = {
                type = mkOption {
                  type = enum [ "external" "simple" "staggered" "trashcan" ];
                  description = mdDoc ''
                    The type of versioning.
                    See <https://docs.syncthing.net/users/versioning.html>.
                  '';
                };
                fsPath = mkOption {
                  default = "";
                  type = either str path;
                  description = mdDoc ''
                    Path to the versioning folder.
                    See <https://docs.syncthing.net/users/versioning.html>.
                  '';
                };
                params = mkOption {
                  type = attrsOf (either str path);
                  description = mdDoc ''
                    The parameters for versioning. Structure depends on
                    [versioning.type](#opt-services.syncthing.folders._name_.versioning.type).
                    See <https://docs.syncthing.net/users/versioning.html>.
                  '';
                };
              };
            });
          };

          rescanInterval = mkOption {
            type = types.int;
            default = 3600;
            description = lib.mdDoc ''
              How often the folder should be rescanned for changes.
            '';
          };

          type = mkOption {
            type = types.enum [ "sendreceive" "sendonly" "receiveonly" "receiveencrypted" ];
            default = "sendreceive";
            description = lib.mdDoc ''
              Whether to only send changes for this folder, only receive them
              or both. `receiveencrypted` can be used for untrusted devices. See
              <https://docs.syncthing.net/users/untrusted.html> for reference.
            '';
          };

          watch = mkOption {
            type = types.bool;
            default = true;
            description = lib.mdDoc ''
              Whether the folder should be watched for changes by inotify.
            '';
          };

          watchDelay = mkOption {
            type = types.int;
            default = 10;
            description = lib.mdDoc ''
              The delay after an inotify event is triggered.
            '';
          };

          ignorePerms = mkOption {
            type = types.bool;
            default = true;
            description = lib.mdDoc ''
              Whether to ignore permission changes.
            '';
          };

          ignoreDelete = mkOption {
            type = types.bool;
            default = false;
            description = mdDoc ''
              Whether to skip deleting files that are deleted by peers.
              See <https://docs.syncthing.net/advanced/folder-ignoredelete.html>.
            '';
          };
        };
      }));
    };

    guiAddress = mkOption {
      default = "http://localhost:8384";
      type = types.str;
      description = mdDoc ''
        URL for syncthing API.
      '';
    };
  };
  config = {
    services.syncthing.enable = lib.mkDefault (folders != []);
    systemd.user.services.syncthing-init = mkIf enabled {
      Install.WantedBy = [ "syncthing.service" ];
      Service = {
        RemainAfterExit = true;
        RuntimeDirectory = "syncthing-init";
        Type = "oneshot";
        ExecStart = "${updateConfig}";
      };
      Unit = {
        After = [ "syncthing.service" ];
        Description = "Syncthing configuration updater";
        Requires = [ "syncthing.service" ];
      };
    };
  };
}
