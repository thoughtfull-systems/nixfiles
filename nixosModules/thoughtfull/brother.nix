{ config, lib, pkgs, ... }: lib.mkIf config.services.printing.enable {
  hardware.printers.ensurePrinters = [
    {
      name = "brother-mfc-l2750dw";
      deviceUri = "ipp://brother.lan:631/";
      model = "everywhere";
      ppdOptions.PageSize = "Letter";
    }
  ];
  services.printing = {
    drivers = [ pkgs.cups-brother-mfcl2750dw ];
  };
}
