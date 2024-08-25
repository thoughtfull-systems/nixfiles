{ config, lib, ... }: let
  desktop = config.thoughtfull.desktop.enable;
in lib.mkIf desktop {
  dconf.settings = with lib.hm.gvariant; {
    "desktop/ibus/general" = {
      embed-preedit-text = lib.mkDefault false;
      engines-order = mkEmptyArray "s";
      preload-engines = mkEmptyArray "s";
      use-global-engine = lib.mkDefault true;
      use-system-keyboard-layout = lib.mkDefault true;
    };
    "desktop/ibus/general/hotkey" = {
      triggers = mkEmptyArray "s";
    };
    "desktop/ibus/panel" = {
      lookup-table-orientation = lib.mkDefault 0;
      show = lib.mkDefault 0;
      show-icon-on-systray = lib.mkDefault false;
      use-custom-font = lib.mkDefault false;
      use-custom-icon = lib.mkDefault false;
      use-custom-theme = lib.mkDefault false;
      use-glyph-from-engine-lang = lib.mkDefault true;
    };
    "desktop/ibus/panel/emoji" = {
      font = lib.mkDefault "Noto Color Emoji 16";
      has-partial-match = lib.mkDefault true;
      hotkey = mkArray "s" [ "<Super>space" ];
      lang = lib.mkDefault "en";
      partial-match-condition = lib.mkDefault 2;
      partial-match-length = lib.mkDefault 3;
      unicode-hotkey = mkArray "s" [ "<Super>u" ];
    };
  };
}
