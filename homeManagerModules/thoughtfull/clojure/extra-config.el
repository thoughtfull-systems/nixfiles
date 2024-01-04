(use-package cider
  :after clojure-mode
  :custom ((cider-preferred-build-tool 'clojure-cli)
           (cider-repl-history-file "~/.cider-history")
           (nrepl-log-messages t)))
(use-package clojure-mode-extra-font-locking
  :after clojure-mode)
(use-package flycheck
  :hook (clojure-mode . flycheck-mode))
(use-package flycheck-clj-kondo
  :after (clojure-mode flycheck))
(use-package paredit
  :hook (clojure-mode . paredit-mode))

;; Local Variables:
;; flycheck-disabled-checkers: (emacs-lisp-checkdoc)
;; End:
