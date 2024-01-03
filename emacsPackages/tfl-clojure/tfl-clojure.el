;;; tfl-clojure.el --- Essentials for Clojure development configuration  -*- lexical-binding: t; -*-
;;
;;  Copyright (c) 2024 Paul Stadig
;;
;;  Author: Paul Stadig <paul@thoughtfull.systems>

;;; Commentary:
;;
;; Essentials for Clojure development configuration

;;; Code:


;;; Dependencies
(require 'tfl)
(use-package cider
  :after clojure-mode)
(use-package clojure-mode-extra-font-locking
  :after clojure-mode)
(use-package flycheck
  :hook (clojure-mode . flycheck-mode))
(use-package flycheck-clj-kondo
  :after (clojure-mode flycheck))
(use-package paredit
  :hook (clojure-mode . paredit-mode))


;;; Configuration
(tfl-custom-theme-set-variables
 'tfl-clojure
 '(cider-preferred-build-tool 'clojure-cli)
 '(cider-repl-history-file "~/.cider-history")
 '(nrepl-log-messages t))

(provide 'tfl-clojure)
;;; tfl-clojure.el ends here
