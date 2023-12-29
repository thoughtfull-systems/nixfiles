epkgs: {
  pjs-clojure = epkgs.elpaBuild {
    packageRequires = with epkgs; [
      cider
      clojure-mode
      clojure-mode-extra-font-locking
      flycheck-clj-kondo
    ];
    pname = "pjs-clojure";
    src = ./pjs-clojure.el;
    version = "0.0.0";
  };
}
