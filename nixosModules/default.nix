inputs: rec {
  default = import ./thoughtfull inputs;
  home = inputs.self.homeManagerModules;
}
