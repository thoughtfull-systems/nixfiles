{ lib, ... }: {
  programs.zsh = lib.mkDefault {
    autocd = true;
    enableAutosuggestions = true;
    enableCompletion = true;
    syntaxHighlighting.enable = true;
    defaultKeymap = "emacs";
    dirHashes = {
      h = "$HOME";
      e = "$HOME/.config/emacs";
      s = "$HOME/src";
    };
    history = {
      # Include timing information in history
      extended = true;
      ignoreDups = true;
      share = false;
    };
    initExtra = ''
      unalias run-help
      autoload run-help

      ## Configuration
      # allow using hash dirs with out a ~ prefix
      setopt CDABLE_VARS
      # corrections based on Dvorak keyboard
      setopt DVORAK
      # immediately append commands to history
      setopt INC_APPEND_HISTORY_TIME
    '';
    shellAliases = {
      help = "run-help";
      # rerun last command piped to less
      l = "fc -e- | less";
    };
  };
}
