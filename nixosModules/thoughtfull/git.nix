{ lib, ... }: {
  programs.git = lib.mkDefault {
    config = {
      init.defaultBranch = "main";
      pull.rebase = false;
    };
    enable = true;
  };
}
