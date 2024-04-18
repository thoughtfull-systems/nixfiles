{ config, lib, pkgs, ... }: let
  cfg = config.thoughtfull.gnome-terminal;
in {
  options.thoughtfull.gnome-terminal.enable = lib.mkEnableOption "gnome-terminal";
  config = lib.mkIf cfg.enable {
    home.packages = [ pkgs.gnome.gnome-terminal ];
    dconf.settings = {
      "org/gnome/terminal/legacy" = {
        default-show-menubar = false;
      };
      "org/gnome/terminal/legacy/keybindings" = {
        close-tab = "disabled";
        close-window = "disabled";
        copy = "disabled";
        find = "disabled";
        find-clear = "disabled";
        find-next = "disabled";
        find-previos = "disabled";
        move-tab-left = "disabled";
        move-tab-right = "disabled";
        new-tab = "disabled";
        new-window = "<Primary><Shift>n";
        next-tab = "disabled";
        prev-tab = "disabled";
        switch-to-tab-1 = "disabled";
        switch-to-tab-10 = "disabled";
        switch-to-tab-2 = "disabled";
        switch-to-tab-3 = "disabled";
        switch-to-tab-4 = "disabled";
        switch-to-tab-5 = "disabled";
        switch-to-tab-6 = "disabled";
        switch-to-tab-7 = "disabled";
        switch-to-tab-8 = "disabled";
        switch-to-tab-9 = "disabled";
        toggle-menubar = "F10";
        zoom-in = "<Primary>plus";
        zoom-normal = "<Primary>0";
        zoom-out = "<Primary>minus";
      };
      "org/gnome/terminal/legacy/profiles:/:b1dcc9dd-5262-4d8d-a863-c897e6d979b9" = {
        bold-is-bright = true;
        custom-command = "zsh -c '(tmux attach || tmux new-session) &>/dev/null'";
        scrollbar-policy = "never";
        use-custom-command = true;
        use-theme-colors = false;
        visible-name = "Default";
      };
    };
    programs.tmux.enable = true;
  };
}
