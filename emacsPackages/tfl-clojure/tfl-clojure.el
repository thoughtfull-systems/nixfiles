;;; tfl-clojure.el --- Essentials for pleasant Clojure development       -*- lexical-binding: t; -*-
;;
;; Copyright (c) 2024 Paul Stadig
;;
;; Author: Paul Stadig <paul@thoughtfull.systems>
;;
;;; Commentary:
;;
;; Configures essential of the Clojure development experience like cider, kondo, etc.
;;
;;; Code:

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

(provide 'tfl-clojure)
;;; tfl-clojure.el ends here
