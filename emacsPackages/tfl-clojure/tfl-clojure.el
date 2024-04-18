;;; tfl-clojure.el --- Essentials for pleasant Clojure development       -*- lexical-binding: t; -*-
;;
;; Copyright (c) 2024 Technosophist
;;
;; Author: Technosophist <technosophist@thoughtfull.systems>
;;
;;; Commentary:
;;
;; Configures essential of the Clojure development experience like cider, kondo, etc.
;;
;;; Code:

(require 'tfl)

(use-package cider
  :after clojure-mode
  :custom ((cider-preferred-build-tool 'clojure-cli)
           (cider-repl-history-file "~/.cider-history")
           (nrepl-log-messages t)))
(use-package clojure-mode
  ;; clojure-mode fills to fill-column plus 2.  I think it is because it narrows the buffer to the
  ;; docstring, which removes the first two spaces on the line, so I'm adjusting for that here.
  :custom ((clojure-docstring-fill-column (- fill-column 2))
           (clojure-indent-style 'always-indent))
  :defer)
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
