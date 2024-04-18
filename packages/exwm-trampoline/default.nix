nixpkgs:
nixpkgs.concatTextFile {
  destination = "/bin/exwm-trampoline";
  executable = true;
  files = [ ./exwm-trampoline ];
  name = "exwm-trampoline";
}
