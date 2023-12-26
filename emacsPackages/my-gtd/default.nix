epkgs: {
  my-gtd = epkgs.trivialBuild {
    pname = "my-gtd";
    src = ./src;
    version = "0.0.0";
  };
}
