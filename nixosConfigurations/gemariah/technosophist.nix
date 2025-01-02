{ pkgs, secrets, ... }: {
  home-manager.users.technosophist = {
    home = {
      file.".config/emacs/init.el".source = ./init.el;
      stateVersion = "24.11";
    };
    programs = {
      emacs.extraPackages = epkgs: with epkgs; [
        tfl-gtd
        tfl-ol-obsidian
        tfl-org
        tfl-org-bullets
        tfl-org-capture
        tfl-org-faces
      ];
      git = {
        enable = true;
        userEmail = "technosophist@thoughtfull.systems";
        userName = "technosophist";
      };
      zsh.enable = true;
    };
    thoughtfull = {
      clojure.enable = true;
      gnome-terminal.enable = true;
      javascript = {
        enable = true;
        nodejs-package = pkgs.nodejs_18;
      };
    };
    xfconf.settings.pointers."TPPS2_Elan_TrackPoint/Acceleration" = 6.5;
  };
  users.users.technosophist = {
    description = "technosophist";
    extraGroups = [ "networkmanager" "video" "wheel" ];
    group = "users";
    isNormalUser = true;
    shell = pkgs.zsh;
    uid = 1000;
  };
}
