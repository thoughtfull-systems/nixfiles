;;; tfl-exwm.el --- Essentials for a pleasant EXWM experience            -*- lexical-binding: t; -*-
;;
;; Copyright (c) 2024 Technosophist
;;
;; Author: Technosophist <technosophist@thoughtfull.systems>
;;
;;; Commentary:
;;
;; Configures essential of the EXWM experience like key bindings and such.
;;
;;; Code:

(use-package exwm
  :custom ((exwm-replace t)
           (exwm-update-class-hook '(tfl-exwm-rename-current-buffer)))
  :defer)
(use-package exwm-input
  :after tfl-exwm-core
  :custom ((exwm-input-global-keys
            ;; C- and M- seem to be fine as modifiers for s- but not S- since
            ;; it works inconsistently between Emacs buffers and X buffers.
            `(([?\s-a]    . tfl-exwm-switch-to-agenda)
              ([?\s-b]    . tfl-switch-to-most-recent-buffer)
              ([?\s-d]    . tfl-exwm-switch-and-cycle-or-run-discord)
              ([?\s-f]    . tfl-exwm-switch-and-cycle-or-run-firefox)
              ([?\s-o]    . tfl-exwm-switch-and-cycle-or-run-obsidian)
              ([?\s-s]    . tfl-exwm-switch-and-cycle-or-run-slack)
              ([?\s-t]    . tfl-exwm-switch-and-cycle-or-run-terminal)
              ([?\C-\s-r] . tfl-exwm-restart)
              ([?\s-&]    . tfl-exwm-run-command)
              ;; Align workspaces more intuitively with key bindings
              ([?\s-1]    . tfl-exwm-switch-to-workspace-1)
              ([?\s-2]    . tfl-exwm-switch-to-workspace-2)
              ([?\s-3]    . tfl-exwm-switch-to-workspace-3)
              ([?\s-4]    . tfl-exwm-switch-to-workspace-4)
              ([?\s-5]    . tfl-exwm-switch-to-workspace-5)
              ([?\s-6]    . tfl-exwm-switch-to-workspace-6)
              ([?\s-7]    . tfl-exwm-switch-to-workspace-7)
              ([?\s-8]    . tfl-exwm-switch-to-workspace-8)
              ([?\s-9]    . tfl-exwm-switch-to-workspace-9)
              ([?\s-0]    . tfl-exwm-switch-to-workspace-0)
              ([print]    . tfl-exwm-run-screenshooter)))
           (exwm-input-simulation-keys
            '(([?\C-_]       . [?\C-z])
              ([?\C-a]       . [home])
              ([?\C-b]       . [left])
              ([?\C-d]       . [delete])
              ([?\C-e]       . [end])
              ([?\C-f]       . [right])
              ([?\C-g]       . [escape])
              ([?\C-k]       . [S-end delete])
              ([?\C-n]       . [down])
              ([?\C-p]       . [up])
              ([?\C-v]       . [next])
              ([?\C-w]       . [\C-x])
              ([?\C-x ?h]    . [?\C-a])
              ([?\C-y]       . [?\C-v])
              ([?\M-<]       . [C-home])
              ([M-backspace] . [C-backspace])
              ([?\M->]       . [C-end])
              ([?\M-b]       . [C-left])
              ([?\M-f]       . [C-right])
              ([?\M-v]       . [prior])
              ([?\M-w]       . [?\C-c])
              ([?\s-w]       . [?\C-w]))))
  :defer)
(use-package exwm-manage
  :custom
  (exwm-manage-configurations
   '(((string-equal exwm-class-name "Gnome-terminal")
      simulation-keys
      (([C-iso-lefttab] . [?\C-z ?\M-b])
       ([C-tab]         . [?\C-z ?\M-f])
       ([?\C-g]         . [?\C-c])
       ([?\C-y]         . [?\C-\S-v])
       ([?\M-<]         . [home])
       ([M-iso-lefttab] . [?\C-z ?\C-\M-B])
       ([M-tab]         . [?\C-z ?\C-\M-F])
       ([?\M->]         . [end])
       ([?\s-w]         . [?\C-z ?K])))
     ((string-equal exwm-class-name "Xfce4-terminal")
      simulation-keys
      (([C-iso-lefttab] . [?\C-z ?\M-b])
       ([C-tab]         . [?\C-z ?\M-f])
       ([?\C-g]         . [?\C-c])
       ([?\C-y]         . [?\C-\S-v])
       ([M-iso-lefttab] . [?\C-z ?\C-\M-B])
       ([M-tab]         . [?\C-z ?\C-\M-F])
       ([?\s-w]         . [?\C-z ?K])))))
  :defer)
(use-package exwm-modeline
  :custom (exwm-modeline-mode t)
  :custom-face
  (exwm-modeline-empty-workspace ((t)) face-defface-spec)
  :defer)
(use-package exwm-layout
  :custom ((exwm-layout-show-all-buffers t))
  :defer)
(use-package exwm-randr
  :custom ((exwm-randr-screen-change-hook 'tfl-exwm-autorandr-change)
           (exwm-randr-workspace-monitor-plist '(9 "eDP-1")))
  :defer)
(use-package exwm-workspace
  :custom ((exwm-workspace-index-map 'tfl-exwm-workspace-name)
           (exwm-workspace-number 10)
           (exwm-workspace-show-all-buffers t))
  :defer)

(require 'tfl-exwm-core)

(provide 'tfl-exwm)
;;; tfl-exwm.el ends here
