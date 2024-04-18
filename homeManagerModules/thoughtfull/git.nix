{ lib, ... }: lib.mkDefault {
  programs.git = {
    enable = true;
    ignores = [ "*~" ];
  };
}
