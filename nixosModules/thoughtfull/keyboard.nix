{ ... }: {
  console.useXkbConfig = lib.mkDefault true;
  services.xserver.xkb = {
    layout = lib.mkDefault "us";
    options = lib.mkDefault "grp:shifts_toggle,ctrl:nocaps,compose:rctrl";
    variant = lib.mkDefault "dvorak";
  };
}
