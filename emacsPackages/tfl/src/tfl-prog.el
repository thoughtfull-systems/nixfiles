;;; tfl-prog.el --- Essentials for a pleasant programming experience     -*- lexical-binding: t; -*-
;;
;; Copyright (c) 2024 Paul Stadig
;;
;; Author: Paul Stadig <paul@thoughtfull.systems>
;;
;;; Commentary:
;;
;; Configures essentials and/or global aspects of the Emacs programming experience including spell
;; checking, flycheck, paredit, etc.
;;
;;; Code:


;;; Dependencies
(require 'tfl-core)
(use-package checkdoc
  ;; since global-flycheck-mode is enabled, there's nothing else to do
  :after elisp-mode)
(use-package company
  :diminish
  :hook (prog-mode . company-mode))
(use-package simple
  :hook (prog-mode . column-number-mode))
(use-package display-line-numbers
  :hook (prog-mode . display-line-numbers-mode))
(use-package eldoc
  :diminish
  :hook (emacs-lisp-mode . eldoc-mode))
(use-package flycheck
  ;; global-flycheck-mode enabled below
  :bind (:map prog-mode-map
              ("C-c e n" . flycheck-next-error)
              ("C-c e p" . flycheck-previous-error))
  :diminish)
(use-package flyspell
  :diminish
  :hook (prog-mode . flyspell-prog-mode))
(use-package magit-extras
  :after magit)
(use-package paredit
  :diminish
  :hook (emacs-lisp-mode . paredit-mode))


;;; Configuration
(tfl-custom-theme-set-variables
 'tfl
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

(provide 'tfl-prog)
;;; tfl-prog.el ends here
