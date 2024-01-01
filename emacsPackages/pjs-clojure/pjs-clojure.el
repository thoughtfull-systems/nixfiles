;;; pjs-clojure.el --- Essentials for Clojure development configuration  -*- lexical-binding: t; -*-
;;
;;  Copyright (c) 2024 Paul Stadig
;;
;;  Author: Paul Stadig <paul@thoughtfull.systems>

;;; Commentary:
;;
;; Essentials for Clojure development configuration

;;; Code:


;;; Dependencies
(require 'pjs)
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
(pjs-custom-theme-set-variables
 'pjs-clojure
 '(cider-preferred-build-tool 'clojure-cli)
 '(cider-repl-history-file "~/.cider-history")
 '(nrepl-log-messages t))

(provide 'pjs-clojure)
;;; pjs-clojure.el ends here
