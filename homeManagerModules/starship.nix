{ pkgs, ... }: {
  home.packages = [ pkgs.starship ];
  programs.zsh.initContent = ''
    eval "$(starship init zsh)"
  '';
}
