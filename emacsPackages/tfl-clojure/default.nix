epkgs: {
  tfl-clojure = epkgs.trivialBuild {
    packageRequires = with epkgs; [
      cider
      clojure-mode
      clojure-mode-extra-font-locking
      flycheck
      flycheck-clj-kondo
      tfl
    ];
    pname = "tfl-clojure";
    src = ./tfl-clojure.el;
    version = "0";
  };
}
