;;; tfl-aider.el --- Essentials for a pleasant aider experience       -*- lexical-binding: t; -*-
;;
;; Copyright (c) technosophist
;;
;; Author: technosophist <technosophist@thoughtfull.systems>
;;
;;; Commentary:
;;
;; Configures essential of the aider experience.
;;
;;; Code:

(use-package aidermacs
  :bind (("C-c a" . aidermacs-transient-menu))
  :config
  ; Set API_KEY in .bashrc, that will automatically picked up by aider or in elisp
  ;; (setenv "ANTHROPIC_API_KEY" "sk-...")
  ; defun my-get-openrouter-api-key yourself elsewhere for security reasons
  ;; (setenv "OPENROUTER_API_KEY" (my-get-openrouter-api-key))
  :custom
  ; See the Configuration section below
  (aidermacs-use-architect-mode t)
  (aidermacs-default-model "sonnet"))

(provide 'tfl-aider)
;;; tfl-aider.el ends here
