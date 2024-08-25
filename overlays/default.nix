inputs: {
  default = final: prev: {
    thoughtfull = inputs.self.packages.${prev.system};
  };
  unstable = final: prev: {
    unstable = import inputs.unstable {
      inherit (final) config system;
    };
  };
}
