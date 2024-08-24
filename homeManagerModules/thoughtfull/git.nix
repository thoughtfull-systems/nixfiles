{ lib, ... }: {
  programs.git = {
    enable = lib.mkDefault true;
    ignores = [ "*~" ];
  };
}
