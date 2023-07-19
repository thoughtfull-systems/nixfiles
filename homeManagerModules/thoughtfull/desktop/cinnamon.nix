{ config, lib, osConfig, pkgs, unstable, ... }: let
  enable = config.thoughtfull.desktop.enable &&
           osConfig.services.xserver.desktopManager.cinnamon.enable;
in lib.mkIf enable {
  # these should all be lib.mkDefault
  dconf.settings = with lib.hm.gvariant; {
    "org/cinnamon" = {
      desktop-effects-workspace = false;
    };
    "org/cinnamon/cinnamon-session" = {
      prefer-hybrid-sleep = true;
      suspend-then-hibernate = false;
    };
    "org/cinnamon/desktop/interface" = {
      color-scheme = "default";
      cursor-blink = true;
      cursor-blink-time = 1200;
      cursor-size = 27;
      cursor-theme = "Adwaita";
      font-name = lib.mkDefault "Ubuntu 10";
      gtk-theme = "Mint-Y";
      icon-theme = "Mint-Y";
      text-scaling-factor = 1.0;
    };
    "org/cinnamon/desktop/media-handling" = {
      automount = false;
      automount-open = false;
      autorun-never = false;
    };
    "org/cinnamon/desktop/notifications" = {
      display-notifications = false;
    };
    "org/cinnamon/desktop/peripherals/keyboard" = {
      delay = mkUint32 532;
      repeat = true;
      repeat-interval = mkUint32 34;
    };
    "org/cinnamon/desktop/peripherals/mouse" = {
      accel-profile = "adaptive";
      double-click = 396;
      drag-threshold = 8;
      natural-scroll = false;
      speed = 0.0;
    };
    "org/cinnamon/desktop/peripherals/touchpad" = {
      click-method = "none";
      disable-while-typing = false;
      two-finger-scrolling-enabled = true;
      natural-scroll = false;
      speed = 0.0;
      tap-to-click = false;
      send-events = true;
    };
    "org/cinnamon/desktop/screensaver" = {
      lock-enabled = true;
      lock-delay = mkUint32 0;
    };
    "org/cinnamon/desktop/session" = {
      idle-delay = mkUint32 0; # Disable screensaver
    };
    "org/cinnamon/desktop/wm/preferences" = {
      theme = "Adapta-Nokto";
      titlebar-font = "Ubuntu Medium 10";
    };
    "org/cinnamon/settings-daemon/plugins/power" = {
      button-power = "interactive";
      critical-battery-action = "hibernate"; #??
      idle-dim-battery = true;
      idle-dim-time = 90;
      idle-brightness = 30;
      lid-close-ac-action = "nothing";
      lid-close-battery-action = "suspend";
      lid-close-suspend-with-external-monitor = false;
      lock-on-suspend = true;
      sleep-display-ac = 3600;
      sleep-display-battery = 900;
      sleep-inactive-ac-timeout = 0;
      sleep-inactive-battery-timeout = 1800;
    };
    "org/cinnamon/settings-daemon/plugins/xsettings" = {
      antialiasing = "rgba";
      hinting = lib.mkDefault "slight";
      rgba-order = "rgb";
    };
    "org/cinnamon/theme" = {
      name = "Mint-Y";
    };
    "org/gnome/desktop/interface" = {
      document-font-name = lib.mkDefault "Ubuntu 10";
      monospace-font-name = lib.mkDefault "Source Code Pro 10";
    };
    "org/nemo/desktop" = {
      desktop-layout = "false::false";
      font = lib.mkDefault "Ubuntu 10";
    };
    "org/nemo/preferences" = {
      close-device-view-on-device-eject = true;
    };
  };
  home.packages = with pkgs; [
    arandr
    autorandr
    b612
    cascadia-code
    fira
    fira-code
    fira-mono
    # input-fonts
    monoid
    source-code-pro
  ];
  programs = {
    emacs.extraConfig = "(load-theme 'tango)";
  };
  thoughtfull = {
    gnome-terminal.enable = true;
    notifications.enable = true;
    yabar.enable = true;
  };
}
