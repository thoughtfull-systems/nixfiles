{ thoughtfull, ... }: {
  home.stateVersion = "22.05";
  imports = [
    thoughtfull.paul
    thoughtfull.syncthing
  ];
  programs = {
    git = {
      userEmail = "paul@stadig.name";
      userName = "Paul Stadig";
    };
  };
  thoughtfull.services.syncthing-init.folders = {
    archive = {
      devices = [ "carbon" "hemera" ];
      enable = true;
    };
    obsidian = {
      devices = [ "carbon" "hemera" "pixel" "pixel5a" ];
      enable = true;
    };
    org = {
      devices = [ "carbon" "hemera" "pixel" "pixel5a" ];
      enable = true;
    };
    sync = {
      devices = [ "bennu" "carbon" "hemera" "pixel" "pixel5a" ];
      enable = true;
    };
  };
}
