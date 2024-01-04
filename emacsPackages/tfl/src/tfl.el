;;; tfl.el --- Essentials for a pleasant Emacs experience                -*- lexical-binding: t; -*-
;;
;; Copyright (c) 2024 Paul Stadig
;;
;; Author: Paul Stadig <paul@thoughtfull.systems>
;;
;;; Commentary:
;;
;; Configures essential and/or global aspects of the Emacs experience.  Customizations broadly
;; affecting the Emacs experience should be here.  As well, this is kind of a "miscellaneous"
;; library for customizations not having a home elsewhere, though the ideally customizations should
;; move out to other libraries as seams become apparent.
;;
;;; Code:


;;; Dependencies
(require 'use-package)
(require 'tfl-core)
(use-package all-the-icons
  :if (display-graphic-p))
(use-package all-the-icons-dired
  :after all-the-icons
  :hook (dired-mode . all-the-icons-dired-mode))
(use-package all-the-icons-ibuffer
  :after all-the-icons
  :hook (ibuffer-mode . all-the-icons-ibuffer-mode))
(use-package consult-org
  :after org
  :bind ("C-c o j j" . consult-org-agenda))
(use-package face-remap
  :defer t
  :diminish buffer-face-mode)
(use-package flyspell
  :hook ((text-mode . flyspell-mode)
         (prog-mode . flyspell-prog-mode)))
(use-package wgrep)
(use-package whitespace
  :diminish global-whitespace-mode)
(use-package writegood-mode
  :diminish
  :hook (text-mode . writegood-mode))


;;; Functions
(defun tfl-buffer-compare (b1 b2)
  "Compare buffer names of B1 and B2."
  (string-collate-lessp (buffer-name b1) (buffer-name b2) nil t))

(defun tfl-switch-buffer (&optional prefix)
  "Switch buffers using incremental completion.
When given PREFIX, use `ibuffer' instead."
  (interactive "p")
  (if (eq prefix 4)
      (ibuffer)
    (call-interactively #'switch-to-buffer)))

(defun tfl-kill-buffer-and-delete-file ()
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


;;; Key bindings
(bind-keys ("C-c D" . tfl-kill-buffer-and-delete-file))
(bind-keys ("C-h C-c" . finder-commentary))
(bind-keys ("C-x b" . tfl-switch-buffer)
           ("C-x C-b" . tfl-switch-buffer))
(bind-keys ("C-x C-l" . find-library))


;;; Configuration
(tfl-custom-theme-set-variables
 'tfl
 '(all-the-icons-dired-monochrome nil)
 '(auto-save-visited-mode t)
 '(backup-directory-alist '(("." . "~/.config/emacs/backups")))
 '(desktop-restore-frames nil)
 '(fringe-mode 1)
 '(global-whitespace-mode t)
 '(indent-tabs-mode nil)
 '(inhibit-startup-echo-area-message (getenv "USER"))
 '(inhibit-startup-screen t)
 '(initial-major-mode 'fundamental-mode)
 '(initial-scratch-message nil)
 '(menu-bar-mode nil)
 '(pulse-delay 2)
 '(pulse-flag nil)
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
(tfl-custom-theme-set-faces
 'tfl
 '(default ((t (:height 110 :family "Source Code Pro"))))
 '(pulse-highlight-start-face ((t (:inherit highlight :extend t)))))

(require 'tfl-completion)
(require 'tfl-prog)

(provide 'tfl)
;;; tfl.el ends here
