{ ... }: {
  fileSystems = {
    "/" = {
      device = "/dev/disk/by-uuid/d78473e5-e415-40be-a0b7-a59f451b042f";
      fsType = "ext4";
      options = [ "noatime" "nodiratime" "discard" ];
    };
    "/boot" = {
      device = "/dev/disk/by-uuid/eaf656ae-fb96-4ebe-95a9-ce7dc991782b";
      fsType = "ext4";
      options = [ "noatime" "nodiratime" ];
    };
    "/boot/firmware" = {
      device = "/dev/disk/by-uuid/2178-694E";
      fsType = "vfat";
      options = [ "noatime" "nodiratime" ];
    };
    "/nix" = {
      device = "/dev/disk/by-uuid/8bff918b-d86d-4f26-b3ae-85b95e7225bc";
      fsType = "ext4";
      options = [ "noatime" "nodiratime" ];
    };
  };
  swapDevices = [{
    device = "/dev/disk/by-uuid/2dff7c80-387a-4e70-9fa3-9f8139c03b6f";
  }];
}
