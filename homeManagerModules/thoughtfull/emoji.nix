{ config, lib, ... }: lib.mkIf config.thoughtfull.desktop.enable {
  dconf.settings = with lib.hm.gvariant; {
    "desktop/ibus/general" = {
      embed-preedit-text =  false;
      engines-order = mkEmptyArray "s";
      preload-engines = mkEmptyArray "s";
      use-global-engine = true;
      use-system-keyboard-layout = true;
    };
    "desktop/ibus/general/hotkey" = {
      triggers = mkEmptyArray "s";
    };
    "desktop/ibus/panel" = {
      lookup-table-orientation = 0;
      show = 0;
      show-icon-on-systray = false;
      use-custom-font =  false;
      use-custom-icon = false;
      use-custom-theme = false;
      use-glyph-from-engine-lang = true;
    };
    "desktop/ibus/panel/emoji" = {
      font = "Noto Color Emoji 16";
      has-partial-match = true;
      hotkey = mkArray "s" [ "<Super>space" ];
      lang = "en";
      partial-match-condition = 2;
      partial-match-length = 3;
      unicode-hotkey = mkArray "s" [ "<Super>u" ];
    };
  };
}
