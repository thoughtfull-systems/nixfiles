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
  :defer
  :diminish buffer-face-mode)
(use-package faces
  :custom-face (default ((t (:height 110 :family "Source Code Pro"))))
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
  :custom-face (pulse-highlight-start-face ((t (:inherit highlight :extend t))))
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
  :defer)
(use-package startup
  :custom ((inhibit-startup-echo-area-message (getenv "USER"))
           (inhibit-startup-screen t)
           (initial-major-mode 'fundamental-mode)
           (initial-scratch-message nil))
  :defer)
(use-package tfl
  :autoload tfl-buffer-compare
  :bind (("C-c D" . tfl-kill-buffer-and-delete-file)
         ("C-x b" . tfl-switch-buffer)
         ("C-x C-b" . tfl-switch-buffer)))
(use-package tool-bar
  :custom (tool-bar-mode nil)
  :defer)
(use-package wgrep
  :defer)
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

;; Local Variables:
;; flycheck-disabled-checkers: (emacs-lisp-checkdoc)
;; End:
