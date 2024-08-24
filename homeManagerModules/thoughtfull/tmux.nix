{ config, lib, pkgs, ... }: let
  desktop = config.thoughtfull.desktop.enable;
  cfg = config.programs.tmux;
  tmux = "${pkgs.tmux}/bin/tmux";
  xsel = "${pkgs.xsel}/bin/xsel";
in {
  home = {
    # TODO: run this on config change instead of activation
    activation = lib.mkIf cfg.enable {
      reloadTmuxConfig = lib.hm.dag.entryAfter ["writeBoundary"] ''
        export TMUX_TMPDIR=/run/user/$UID
        if ${tmux} ls &> /dev/null; then
          echo "Reloading tmux config"
          $DRY_RUN_CMD ${tmux} source-file $HOME/.config/tmux/tmux.conf
        else
          echo "NOT Reloading tmux config (tmux not running)"
        fi
      '';
    };
  };
  programs.tmux = {
    baseIndex = lib.mkDefault 1;
    clock24 = lib.mkDefault true;
    enable = lib.mkDefault desktop;
    extraConfig = ''
      set-option -g alternate-screen on
      set-option -g copy-command "${xsel} -i --clipboard"
      set-option -g detach-on-destroy off
      set-option -g history-file "~/.config/tmux/history"
      set-option -g monitor-activity on
      set-option -g monitor-bell on
      set-option -g mouse on
      set-option -g remain-on-exit failed
      set-option -g renumber-windows on
      set-option -g repeat-time 2000
      set-option -g status-position top
      set-option -g window-status-current-style reverse
      set-option -g window-status-activity-style bg=yellow

      bind-key -T copy-mode -N "Copy to clipboard and cancel" C-w send -X copy-pipe-and-cancel
      bind-key -T copy-mode -N "Show copy-mode key bindings in a new pane" ? split-window -dfh "tmux list-keys -NT copy-mode | less"
      bind-key -T copy-mode DoubleClick1Pane  select-pane \; send-keys -X select-word \; run-shell -d 0.3 \; send-keys -X copy-pipe-and-cancel
      bind-key -T copy-mode -N "Select window 10" M-0 select-window -t :=10
      bind-key -T copy-mode -N "Select window 1" M-1 select-window -t :=1
      bind-key -T copy-mode -N "Select window 2" M-2 select-window -t :=2
      bind-key -T copy-mode -N "Select window 3" M-3 select-window -t :=3
      bind-key -T copy-mode -N "Select window 4" M-4 select-window -t :=4
      bind-key -T copy-mode -N "Select window 5" M-5 select-window -t :=5
      bind-key -T copy-mode -N "Select window 6" M-6 select-window -t :=6
      bind-key -T copy-mode -N "Select window 7" M-7 select-window -t :=7
      bind-key -T copy-mode -N "Select window 8" M-8 select-window -t :=8
      bind-key -T copy-mode -N "Select window 9" M-9 select-window -t :=9
      bind-key -T copy-mode -N "Copy to clipboard" M-w send -X copy-pipe
      bind-key -T copy-mode MouseDragEnd1Pane send -X copy-pipe-and-cancel
      bind-key -T copy-mode TripleClick1Pane  select-pane \; send-keys -X select-line \; run-shell -d 0.3 \; send-keys -X copy-pipe-and-cancel

      bind-key -n -N "Select window 10" M-0 select-window -t :=10
      bind-key -n -N "Select window 1" M-1 select-window -t :=1
      bind-key -n -N "Select window 2" M-2 select-window -t :=2
      bind-key -n -N "Select window 3" M-3 select-window -t :=3
      bind-key -n -N "Select window 4" M-4 select-window -t :=4
      bind-key -n -N "Select window 5" M-5 select-window -t :=5
      bind-key -n -N "Select window 6" M-6 select-window -t :=6
      bind-key -n -N "Select window 7" M-7 select-window -t :=7
      bind-key -n -N "Select window 8" M-8 select-window -t :=8
      bind-key -n -N "Select window 9" M-9 select-window -t :=9
      bind-key -n TripleClick1Pane select-pane -t = \; if-shell -F "#{||:#{pane_in_mode},#{mouse_any_flag}}" "send -M" "copy-mode -H ; send -X select-line ; run -d0.3 ; send -X copy-pipe-and-cancel"

      bind-key -N "Select the pane to the left of the active pane" C-b select-pane -L
      bind-key -N "Select the pane to the right of the active pane" C-f select-pane -R
      bind-key -N "Select the pane below the active pane" C-n select-pane -D
      bind-key -N "Select the pane above the active pane" C-p select-pane -U
      bind-key -N "Break pane to a new window after current window" ! break-pane -a
      bind-key -N "Run command in a new window (EXWM: s-$)" $ command-prompt -p "$" "new-window \"zsh -ic '%%'\" ; set-option -w remain-on-exit on"
      bind-key -N "Split pane horizontally" "-" split-window -v -c "#{pane_current_path}"
      bind-key -N "Rename current session" @ command-prompt -I "#S" "rename-session -- '%%'"
      bind-key -N "Resize the pane left by 1" -r B resize-pane -L
      bind-key -N "Create a new named window" C command-prompt -p "Name:" "new-window ; rename-window \"%%\""
      bind-key -n DoubleClick1Pane select-pane -t = \; if-shell -F "#{||:#{pane_in_mode},#{mouse_any_flag}}" "send -M" "copy-mode -H ; send -X select-word ; run -d0.3 ; send -X copy-pipe-and-cancel"
      bind-key -N "Resize the pane right by 1" -r F resize-pane -R
      bind-key -N "Layout panes horizontally with main pane" H select-layout main-horizontal
      bind-key -N "Kill current window (EXWM: s-w)" K kill-window
      bind-key -N "Resize the pane down by 1" -r N resize-pane -D
      bind-key -N "Resize the pane up by 1" -r P resize-pane -U
      bind-key -N "Reload ~/.config/tmux/tmux.conf" R run-shell "tmux source-file ~/.config/tmux/tmux.conf > /dev/null; tmux display-message \"Sourced ~/.config/tmux/tmux.conf!\""
      bind-key -N "Layout panes tiled" T select-layout tiled
      bind-key -N "Customize options" U customize-mode -Z
      bind-key -N "Layout panes vertically with main pane" V select-layout main-vertical
      bind-key [ display-message "? for key bindings" \; copy-mode
      bind-key -N "Split window horizontally" "\\" split-window -fh -c "#{pane_current_path}"
      bind-key -N "Split window vertically" "_" split-window -fv -c "#{pane_current_path}"
      bind-key -N "Select the pane to the left of the active pane" b select-pane -L
      bind-key -N "Select the pane to the right of the active pane" f select-pane -R
      bind-key -N "Layout panes spaced evenly horizontally" h select-layout even-horizontal
      bind-key -N "Kill the active pane" k kill-pane
      bind-key -N "Select pane below the active pane" n select-pane -D
      bind-key -N "Select pane above the active pane" p select-pane -U
      bind-key -N "Layout panes tiled" t select-layout tiled
      bind-key -N "Layout panes spaced evenly vertically" v select-layout even-vertical
      bind-key -N "Split pane vertically" "|" split-window -h -c "#{pane_current_path}"
      bind-key -N "Select previous window with alert (EXWM: M-S-Tab)" M-C-b previous-window -a
      bind-key -N "Select next window with alert (EXWM: M-Tab)" M-C-f next-window -a
      bind-key -N "Resize the pane left by 5" -r M-B resize-pane -L 5
      bind-key -N "Resize the pane right by 5" -r M-F resize-pane -R 5
      bind-key -N "Resize the pane down by 5" -r M-N resize-pane -D 5
      bind-key -N "Resize the pane up by 5" -r M-P resize-pane -U 5
      bind-key -N "Select previous window (EXWM: C-S-Tab)" M-b previous-window
      bind-key -N "Select next window (EXWM: C-Tab)" M-f next-window

      unbind-key -T prefix "'"
      unbind-key -T prefix %
      unbind-key -T prefix &
      unbind-key -T prefix '"'
      unbind-key -T prefix 0
      unbind-key -T prefix 1
      unbind-key -T prefix 2
      unbind-key -T prefix 3
      unbind-key -T prefix 4
      unbind-key -T prefix 5
      unbind-key -T prefix 6
      unbind-key -T prefix 7
      unbind-key -T prefix 8
      unbind-key -T prefix 9
      unbind-key -T prefix C-Down
      unbind-key -T prefix C-Left
      unbind-key -T prefix C-Right
      unbind-key -T prefix C-Up
      unbind-key -T prefix Down
      unbind-key -T prefix Left
      unbind-key -T prefix M-1
      unbind-key -T prefix M-2
      unbind-key -T prefix M-3
      unbind-key -T prefix M-4
      unbind-key -T prefix M-5
      unbind-key -T prefix M-Down
      unbind-key -T prefix M-Left
      unbind-key -T prefix M-Right
      unbind-key -T prefix M-Up
      unbind-key -T prefix Right
      unbind-key -T prefix S-Down
      unbind-key -T prefix S-Left
      unbind-key -T prefix S-Right
      unbind-key -T prefix S-Up
      unbind-key -T prefix Space
      unbind-key -T prefix Up
      unbind-key -T prefix l
      unbind-key -T prefix o
      unbind-key -T prefix x
    '';
    keyMode = lib.mkDefault "emacs";
    plugins = with pkgs; [
      {
        plugin = tmuxPlugins.better-mouse-mode;
        extraConfig = "set-option -g @emulate-scroll-for-no-mouse-alternate-buffer on";
      }
      {
        plugin = tmuxPlugins.prefix-highlight;
        extraConfig = ''
          set-option -g @prefix_highlight_show_copy_mode on
          set-option -g @prefix_highlight_show_sync_mode on
          set-option -g status-right "#{?#{==:#{user},root},#[bg=red]#[fg=white],}#{user}@#h#[default]#{prefix_highlight}"
        '';
      }
    ];
    shortcut = lib.mkDefault "z";
    terminal = lib.mkDefault "screen-256color";
  };
}
