;;; init.el --- init script  -*- lexical-binding: t; -*-

;; Copyright (c) 2023 Paul Stadig

;;; Commentary:

;; None

;;; Code:

(require 'use-package)
(use-package my
  :demand t
  ;; Key binding conventions
  ;; https://www.gnu.org/software/emacs/manual/html_node/elisp/Key-Binding-Conventions.html
  ;;
  ;; C-c [letter] is reserved for users
  ;; <f5> through <f9> are reserved for users
  :bind (("C-x b" . my-switch-buffer)
         ("C-x C-b" . my-switch-buffer)
         ("C-c D" . my-kill-buffer-and-delete-file)))
(use-package my-completion)
(use-package my-prog)

(provide 'init)
;;; init.el ends here
