nixpkgs: let
  yubikey-touch-plugin = nixpkgs.replaceVarsWith {
    isExecutable = true;
    replacements = {};
    src = ./yubikey-touch-plugin;
  };
  yubikey-touch-plugin-updater = nixpkgs.replaceVarsWith {
    isExecutable = true;
    replacements = {
      not_waiting = not-waiting;
      waiting = waiting;
    };
    src = ./yubikey-touch-plugin-updater;
  };
  waiting = ./yubikey-waiting.png;
  not-waiting = ./yubikey-not-waiting.png;
in nixpkgs.stdenv.mkDerivation {
  pname = "yubikey-touch-plugin";
  version = "0.0.0";
  builder = nixpkgs.writeShellScript "builder.sh" ''
    source $stdenv/setup
    mkdir -p $out/bin
    cp $yubikey_touch_plugin $out/bin/yubikey-touch-plugin
    cp $yubikey_touch_plugin_updater $out/bin/yubikey-touch-plugin-updater
  '';
  yubikey_touch_plugin = yubikey-touch-plugin;
  yubikey_touch_plugin_updater = yubikey-touch-plugin-updater;
}
