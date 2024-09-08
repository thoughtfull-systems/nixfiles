;;; tfl.el --- Essentials for a pleasant Emacs experience                -*- lexical-binding: t; -*-
;;
;; Copyright (c) 2024 Technosophist
;;
;; Author: Technosophist <technosophist@thoughtfull.systems>
;;
;;; Commentary:
;;
;; Configures essential and/or global aspects of the Emacs experience.  Customizations broadly
;; affecting the Emacs experience should be here.  As well, this is kind of a "miscellaneous"
;; library for customizations not having a home elsewhere, though the ideally customizations should
;; move out to other libraries as seams become apparent.
;;
;;; Code:


;;; General
(use-package all-the-icons
  :if (display-graphic-p))
(use-package all-the-icons-dired
  :after (all-the-icons dired)
  :hook (dired-mode . all-the-icons-dired-mode)
  :custom (all-the-icons-dired-monochrome nil))
(use-package all-the-icons-ibuffer
  :after (all-the-icons ibuffer)
  :hook (ibuffer-mode . all-the-icons-ibuffer-mode))
(use-package desktop
  :custom (desktop-restore-frames nil)
  :defer)
(use-package face-remap
  :custom (buffer-face-mode-face 'variable-pitch-text)
  :defer
  :diminish buffer-face-mode)
(use-package faces
  :custom-face
  (default ((t (:height 110 :family "Source Code Pro"))) face-defface-spec)
  (fixed-pitch ((t (:height 110))))
  (mode-line-inactive ((t (:foreground "gray60"))))
  :defer)
(use-package files
  :custom ((auto-save-visited-mode t)
           (backup-directory-alist '(("." . "~/.config/emacs/backups"))))
  :defer)
(use-package finder
  :bind ("C-h C-c" . finder-commentary))
(use-package find-func
  :bind ("C-x C-l" . find-library))
(use-package fringe
  :custom (fringe-mode 1)
  :defer)
(use-package flyspell
  :hook (text-mode . flyspell-mode))
(use-package menu-bar
  :custom (menu-bar-mode nil)
  :defer)
(use-package paren
  :custom (show-paren-delay 0.25)
  :defer)
(use-package pulse
  :custom ((pulse-delay 2)
           (pulse-flag nil))
  :custom-face (pulse-highlight-start-face ((t (:inherit highlight :extend t))) face-defface-spec)
  :defer)
(use-package safehist
  :custom (savehist-mode t)
  :defer)
(use-package saveplace
  :custom (save-place-mode t)
  :defer)
(use-package scroll-bar
  :custom (scroll-bar-mode nil)
  :defer)
(use-package simple
  :custom ((indent-tabs-mode nil)
           (save-interprogram-paste-before-kill t))
  :hook (visual-fill-column-mode . visual-line-mode)
  :defer)
(use-package startup
  :custom ((inhibit-startup-echo-area-message (getenv "USER"))
           (inhibit-startup-screen t)
           (initial-major-mode 'text-mode)
           (initial-scratch-message nil))
  :defer)
(use-package tfl-core
  :autoload tfl-buffer-compare-name
  :bind (("C-c D" . tfl-kill-buffer-and-delete-file)
         ("C-x b" . tfl-switch-buffer)
         ("C-x C-b" . tfl-switch-buffer)))
(use-package tool-bar
  :custom (tool-bar-mode nil)
  :defer)
(use-package visual-fill-column
  :hook ((markdown-mode org-mode) . visual-fill-column-mode)
  :config
  (advice-add 'text-scale-adjust :after 'visual-fill-column-adjust))
(use-package window
  :custom (split-window-preferred-function 'visual-fill-column-split-window-sensibly)
  :defer)
(use-package wgrep
  :defer)
(use-package which-key
  :custom ((which-key-idle-delay 1.5)
           (which-key-mode t))
  :diminish)
(use-package whitespace
  :custom ((global-whitespace-mode t)
           (whitespace-action '(auto-cleanup))
           (whitespace-global-modes '(prog-mode))
           (whitespace-line-column nil)
           (whitespace-style '(face trailing lines-tail missing-newline-at-eof empty indentation
                                    space-after-tab space-before-tab)))
  :defer
  :diminish global-whitespace-mode)
(use-package writegood-mode
  :diminish
  :hook (text-mode . writegood-mode))


;;; Completion
(use-package all-the-icons-completion
  :after all-the-icons
  :commands (all-the-icons-completion-mode)
  :config (all-the-icons-completion-mode)
  :hook (marginalia-mode . all-the-icons-completion-marginalia-setup))
(use-package consult
  :custom ((consult-after-jump-hook '(recenter pulse-momentary-highlight-one-line))
           (consult-narrow-key "C-,")
           (consult-preview-key nil)))
(use-package consult-org
  :after (consult org)
  :bind (("C-c o j j" . consult-org-agenda)
         :map org-mode-map
         ("C-c C-j" . consult-org-heading)))
(use-package marginalia
  :custom ((marginalia-mode t)))
(use-package minibuffer
  :custom ((completion-auto-help nil)
           (completion-category-overrides '((file (styles basic partial-completion orderless))))
           (completion-cycle-threshold 3)
           (completion-styles '(orderless))
           (completions-detailed t)
           (read-buffer-completion-ignore-case t)
           (read-file-name-completion-ignore-case t))
  :custom-face (completions-common-part ((t (:inherit orderless-match-face-0))) face-defface-spec)
  :defer)
(use-package orderless
  :custom (orderless-matching-styles
           '(orderless-regexp orderless-literal orderless-initialism orderless-prefixes))
  :defer)
(use-package vertico
  :custom ((vertico-mode t)))


;; Programming
(use-package buffer
  :custom (fill-column 100)
  :defer)
(use-package checkdoc
  ;; since global-flycheck-mode is enabled, there's nothing else to do
  :after elisp-mode
  :custom (checkdoc-force-docstrings-flag nil))
(use-package company
  :diminish
  :hook (prog-mode . company-mode))
(use-package simple
  :hook (prog-mode . column-number-mode))
(use-package display-line-numbers
  :hook (prog-mode . display-line-numbers-mode)
  :custom ((display-line-numbers-minor-tick 10)
           (display-line-numbers-width-start t))
  ;; fix line number scaling; minor tick would scale with text-scale-adjust, but major tick would
  ;; not.
  :custom-face
  (line-number-major-tick ((t (:inherit default))))
  (line-number-minor-tick ((t (:inherit default)))))
(use-package eldoc
  :diminish
  :hook (emacs-lisp-mode . eldoc-mode))
(use-package electric
  ;; electric-indent-mode swaps the default behavior of C-j and RET which is confusing, I prefer the
  ;; default behavior.
  :custom (electric-indent-mode nil)
  :defer)
(use-package flycheck
  :bind (:map prog-mode-map
              ("C-c e n" . flycheck-next-error)
              ("C-c e p" . flycheck-previous-error))
  :custom ((flycheck-emacs-lisp-check-declare t)
           (flycheck-emacs-lisp-load-path 'inherit)
           (global-flycheck-mode t))
  :diminish)
(use-package flyspell
  :diminish
  :hook (prog-mode . flyspell-prog-mode))
(use-package lisp-mode
  :custom (emacs-lisp-docstring-fill-column 80)
  :defer)
(use-package magit-extras
  :after magit
  ;; add project-switch-command without demand loading magit
  :preface (advice-add #'project-switch-project :before
                       (lambda (&rest _args)
                         (require 'magit-extras))))
(use-package paredit
  :diminish
  :hook (emacs-lisp-mode . paredit-mode))
(use-package sh-script
  :custom (sh-basic-offset 2)
  :defer)

(provide 'tfl)
;;; tfl.el ends here
