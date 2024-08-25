{ pkgs, ... }: {
  home.packages = [ pkgs.starship ];
  programs.zsh.initExtra = ''
    eval "$(starship init zsh)"
  '';
}
