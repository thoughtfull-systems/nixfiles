{ ... } : {
  # obsidian depends on electron 25, which is EOL
  # https://github.com/NixOS/nixpkgs/issues/273611
  # https://github.com/NixOS/nixpkgs/issues/265125
  nixpkgs.config.permittedInsecurePackages = [ "electron-25.9.0" ];
}
