;;; my.el --- General code and configuration  -*- lexical-binding: t; -*-

;; Copyright (c) 2023 Paul Stadig

;; Version: 0.0.0

;;; Commentary:

;; None

;;; Code:
(defvar my-custom-file "~/.config/emacs/custom.el")

(load my-custom-file t)

(require 'use-package)
(use-package all-the-icons
  :if (display-graphic-p))
(use-package all-the-icons-completion
  :after (all-the-icons icomplete marginalia)
  :commands (all-the-icons-completion-mode)
  :config (all-the-icons-completion-mode)
  :hook (marginalia-mode . all-the-icons-completion-marginalia-setup))
(use-package all-the-icons-dired
  :after (all-the-icons)
  :hook (dired-mode . all-the-icons-dired-mode))
(use-package all-the-icons-ibuffer
  :after (all-the-icons)
  :hook (ibuffer-mode . all-the-icons-ibuffer-mode))
(use-package flyspell
  :hook ((text-mode . flyspell-mode)
         (prog-mode . flyspell-prog-mode)))

(defun my-buffer-compare (b1 b2)
  (string-collate-lessp (buffer-name b1) (buffer-name b2) nil t))

(defun my-switch-buffer (&optional prefix)
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

(deftheme my)
(custom-theme-set-variables
 'my
 '(all-the-icons-dired-monochrome nil)
 '(auto-save-visited-mode t)
 '(backup-directory-alist '(("." . "~/.config/emacs/backups")))
 '(custom-file my-custom-file)
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

(provide 'my)
;;; my.el ends here
