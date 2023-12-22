epkgs: {
  my-clojure = epkgs.elpaBuild {
    packageRequires = with epkgs; [
      cider
      clojure-mode
      clojure-mode-extra-font-locking
      flycheck-clj-kondo
    ];
    pname = "my-clojure";
    src = ./my-clojure.el;
    version = "0.0.0";
  };
}
