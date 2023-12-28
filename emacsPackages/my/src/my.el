;;; my.el --- Essentials for a pleasant Emacs experience                  -*- lexical-binding: t -*-
;;
;; Copyright (c) 2023 Paul Stadig
;;
;; Author: Paul Stadig <paul@thoughtfull.systems>
;;

;;; Commentary:
;;
;; This library is demand loaded because some of it needs to be evaluated very early in loading
;; Emacs, but things that can be deferred should be.
;;

;;; Code:

;; This needs to be done as early as possible.
(defvar my--custom-file "~/.config/emacs/custom.el"
  "Location to use for `custom-file'.  Modification will have no effect.")
(load my--custom-file t)


;;; Dependencies
(use-package all-the-icons
  :if (display-graphic-p))
(use-package all-the-icons-dired
  :after (all-the-icons)
  :hook (dired-mode . all-the-icons-dired-mode))
(use-package all-the-icons-ibuffer
  :after (all-the-icons)
  :hook (ibuffer-mode . all-the-icons-ibuffer-mode))
(use-package flyspell
  :hook ((text-mode . flyspell-mode)
         (prog-mode . flyspell-prog-mode)))
(use-package imenu
  :bind (("C-c i" . imenu)
         ("C-c C-i" . imenu)))
(use-package wgrep)
(use-package writegood-mode
  :hook (text-mode . writegood-mode))


;;; Functions
(defun my-buffer-compare (b1 b2)
  "Compare buffer names of B1 and B2."
  (string-collate-lessp (buffer-name b1) (buffer-name b2) nil t))

(defun my-switch-buffer (&optional prefix)
  "Switch buffers using incremental completion.
When given PREFIX, use `ibuffer' instead."
  (interactive "p")
  (if (eq prefix 4)
      (ibuffer)
    (call-interactively 'switch-to-buffer)))

(defun my-kill-buffer-and-delete-file ()
  "Kill the current buffer and delete the file it is visiting."
  (interactive)
  (let ((filename (buffer-file-name)))
    (when filename
      (if (vc-backend filename)
          (vc-delete-file filename)
        (progn
          (delete-file filename)
          (message "Deleted file %s" filename)
          (kill-buffer))))))


;;; Configuration
(deftheme my)
(custom-theme-set-variables
 'my
 '(all-the-icons-dired-monochrome nil)
 '(auto-save-visited-mode t)
 '(backup-directory-alist '(("." . "~/.config/emacs/backups")))
 '(custom-file my--custom-file)
 '(desktop-restore-frames nil)
 '(fringe-mode 1)
 '(global-whitespace-mode t)
 '(indent-tabs-mode nil)
 '(inhibit-startup-echo-area-message (getenv "USER"))
 '(inhibit-startup-screen t)
 '(menu-bar-mode nil)
 '(save-interprogram-paste-before-kill t)
 '(save-place-mode t)
 '(savehist-mode t)
 '(scroll-bar-mode nil)
 '(show-paren-delay 0.25)
 '(tool-bar-mode nil)
 '(whitespace-action '(auto-cleanup))
 '(whitespace-global-modes '(prog-mode))
 '(whitespace-line-column nil)
 '(whitespace-style '(face trailing lines-tail missing-newline-at-eof empty indentation
                           space-after-tab space-before-tab)))
(custom-theme-set-faces
 'my
 '(default ((t (:height 110 :family "Source Code Pro")))))
(provide-theme 'my)
(enable-theme 'my)

(require 'my-completion)
(require 'my-prog)

(provide 'my)
;;; my.el ends here
