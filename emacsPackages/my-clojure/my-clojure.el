;;; my-clojure.el --- Clojure development configuration  -*- lexical-binding: t; -*-

;;  Copyright (c) 2023 Paul Stadig

;;  Version: 0.0.0

;;; Commentary:

;; None

;;; Code:

(require 'use-package)
(use-package cider
  :after clojure-mode)
(use-package clojure-mode
  :hook ((clojure-mode . paredit-mode)
         (clojure-mode . flycheck-mode)))
(use-package clojure-mode-extra-font-locking
  :after clojure-mode)
(use-package flycheck-clj-kondo
  :after (flycheck-clojure)
  :init (flycheck-clojure-setup))
(use-package flycheck-clojure
  :after (clojure-mode flycheck))

(deftheme my-clojure)
(custom-theme-set-variables
 'my-clojure
 '(cider-preferred-build-tool 'clojure-cli)
 '(cider-repl-history-file "~/.cider-history")
 '(nrepl-log-messages t))
(provide-theme 'my-clojure)
(enable-theme 'my-clojure)
(provide 'my-clojure)
;;; my-clojure.el ends here
