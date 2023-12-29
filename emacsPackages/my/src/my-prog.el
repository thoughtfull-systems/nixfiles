;;; my-prog.el --- Essentials for a pleasant programming experience       -*- lexical-binding: t -*-

;;  Copyright (c) 2023 Paul Stadig
;;
;; Author: Paul Stadig <paul@thoughtfull.systems>
;;

;;; Commentary:
;;
;; This is demand loaded because there's a fair amount of config for prog-mode and emacs-lisp which
;; will just get loaded immediately anyway (because of the scratch buffer), but things that can be
;; deferred should be.
;;

;;; Code:

;;; Dependencies
(require 'my-core)
(use-package checkdoc
  ;; since global-flycheck-mode is enabled, there's nothing else to do
  :after elisp-mode)
(use-package company
  :hook (prog-mode . company-mode))
(use-package simple
  :hook (prog-mode . column-number-mode))
(use-package display-line-numbers
  :hook (prog-mode . display-line-numbers-mode))
(use-package eldoc
  :hook (emacs-lisp-mode . eldoc-mode))
(use-package flycheck
  ;; global-flycheck-mode enabled below
  :bind (:map prog-mode-map
              ("C-c e n" . flycheck-next-error)
              ("C-c e p" . flycheck-previous-error)))
(use-package flyspell
  :hook (prog-mode . flyspell-prog-mode))
(use-package magit-extras
  :after magit)
(use-package paredit
  :hook (emacs-lisp-mode . paredit-mode))


;;; Configuration
(my-custom-theme-set-variables
 'my
 '(checkdoc-force-docstrings-flag nil)
 '(display-line-numbers-minor-tick 10)
 '(display-line-numbers-width-start t)
 ;; electric-indent-mode swaps the default behavior of C-j and RET which is confusing, I prefer the
 ;; default behavior.
 '(electric-indent-mode nil)
 '(emacs-lisp-docstring-fill-column 80)
 '(fill-column 100)
 '(flycheck-emacs-lisp-check-declare t)
 '(flycheck-emacs-lisp-load-path 'inherit)
 '(global-flycheck-mode t)
 '(sh-basic-offset 2))

(provide 'my-prog)
;;; my-prog.el ends here
