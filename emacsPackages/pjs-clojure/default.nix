epkgs: {
  pjs-clojure = epkgs.trivialBuild {
    packageRequires = with epkgs; [
      cider
      clojure-mode
      clojure-mode-extra-font-locking
      flycheck
      flycheck-clj-kondo
      pjs
    ];
    pname = "pjs-clojure";
    src = ./pjs-clojure.el;
    version = "0";
  };
}
