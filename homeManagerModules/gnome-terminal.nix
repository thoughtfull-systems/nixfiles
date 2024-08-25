{ config, lib, pkgs, ... }: let
  cfg = config.thoughtfull.gnome-terminal;
in {
  options.thoughtfull.gnome-terminal.enable = lib.mkEnableOption "gnome-terminal";
  config = lib.mkIf cfg.enable {
    home.packages = [ pkgs.gnome.gnome-terminal ];
    dconf.settings = {
      "org/gnome/terminal/legacy" = {
        default-show-menubar = lib.mkDefault false;
      };
      "org/gnome/terminal/legacy/keybindings" = {
        close-tab = lib.mkDefault "disabled";
        close-window = lib.mkDefault "disabled";
        copy = lib.mkDefault "disabled";
        find = lib.mkDefault "disabled";
        find-clear = lib.mkDefault "disabled";
        find-next = lib.mkDefault "disabled";
        find-previos = lib.mkDefault "disabled";
        move-tab-left = lib.mkDefault "disabled";
        move-tab-right = lib.mkDefault "disabled";
        new-tab = lib.mkDefault "disabled";
        new-window = lib.mkDefault "<Primary><Shift>n";
        next-tab = lib.mkDefault "disabled";
        prev-tab = lib.mkDefault "disabled";
        switch-to-tab-1 = lib.mkDefault "disabled";
        switch-to-tab-10 = lib.mkDefault "disabled";
        switch-to-tab-2 = lib.mkDefault "disabled";
        switch-to-tab-3 = lib.mkDefault "disabled";
        switch-to-tab-4 = lib.mkDefault "disabled";
        switch-to-tab-5 = lib.mkDefault "disabled";
        switch-to-tab-6 = lib.mkDefault "disabled";
        switch-to-tab-7 = lib.mkDefault "disabled";
        switch-to-tab-8 = lib.mkDefault "disabled";
        switch-to-tab-9 = lib.mkDefault "disabled";
        toggle-menubar = lib.mkDefault "F10";
        zoom-in = lib.mkDefault "<Primary>plus";
        zoom-normal = lib.mkDefault "<Primary>0";
        zoom-out = lib.mkDefault "<Primary>minus";
      };
      "org/gnome/terminal/legacy/profiles:/:b1dcc9dd-5262-4d8d-a863-c897e6d979b9" = {
        bold-is-bright = lib.mkDefault true;
        custom-command = lib.mkDefault "zsh -c '(tmux attach || tmux new-session) &>/dev/null'";
        scrollbar-policy = lib.mkDefault "never";
        use-custom-command = lib.mkDefault true;
        use-theme-colors = lib.mkDefault false;
        visible-name = lib.mkDefault "Default";
      };
    };
    programs.tmux.enable = true;
  };
}
