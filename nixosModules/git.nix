{ lib, ... }: {
  programs.git = {
    config = {
      init.defaultBranch = lib.mkDefault "main";
      pull.rebase = lib.mkDefault false;
    };
    enable = lib.mkDefault true;
  };
}
