{ lib, ... }: {
  programs.zsh = {
    autocd = lib.mkDefault true;
    autosuggestion.enable = lib.mkDefault true;
    enableCompletion = lib.mkDefault true;
    syntaxHighlighting.enable = lib.mkDefault true;
    defaultKeymap = lib.mkDefault "emacs";
    dirHashes = {
      h = "$HOME";
      e = "$HOME/.config/emacs";
      s = "$HOME/src";
    };
    history = {
      # Include timing information in history
      extended = lib.mkDefault true;
      ignoreDups = lib.mkDefault true;
      share = lib.mkDefault false;
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
