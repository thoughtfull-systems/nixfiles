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

(require 'auth-source)

(use-package aidermacs
  :bind (("C-c a" . aidermacs-transient-menu))
  :config
  ;; Set API_KEY in .bashrc, that will automatically picked up by aider or in elisp
  (setenv "ANTHROPIC_API_KEY"
          (auth-source-pick-first-password :host "console.anthropic.com"))
  :custom
  ;; See the Configuration section below
  (aidermacs-use-architect-mode t)
  (aidermacs-default-model "claude-3.5"))

(provide 'tfl-aider)
;;; tfl-aider.el ends here
