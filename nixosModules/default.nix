inputs: rec {
  default = thoughtfull;
  home = inputs.self.homeManagerModules;
  thoughtfull = import ./thoughtfull inputs;
}
