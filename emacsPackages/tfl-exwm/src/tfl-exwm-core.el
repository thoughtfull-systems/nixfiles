;;; tfl-exwm-core.el --- Functions and such for customizing EXWM         -*- lexical-binding: t; -*-
;;
;; Copyright (C) 2024 Technosophist
;;
;; Author: Technosophist <technosophist@thoughtfull.systems>
;;
;;; Commentary:
;;
;; Functions for managing workspaces, buffers, and processes with EXWM
;;
;;; Code:


;;; Dependencies
(require 'desktop)
(require 'exwm)
(require 'exwm-randr)
(require 'tfl)
(require 'tfl-gtd-agenda)
(require 'seq)


;;; Variables
(defvar tfl-exmw-agenda-workspace-number 10)

(defvar tfl-exwm-discord-workspace-number 11)
(defvar tfl-exwm-discord-class "discord")
(defvar tfl-exwm-discord-command "discord")

(defvar tfl-exwm-firefox-workspace-number 12)
(defvar tfl-exwm-firefox-class "firefox")
(defvar tfl-exwm-firefox-command "firefox")

(defvar tfl-exwm-obsidian-workspace-number 13)
(defvar tfl-exwm-obsidian-class "obsidian")
(defvar tfl-exwm-obsidian-command "obsidian")

(defvar tfl-exwm-slack-workspace-number 14)
(defvar tfl-exwm-slack-class "Slack")
(defvar tfl-exwm-slack-command "slack")

(defvar tfl-exwm-terminal-workspace-number 15)
(defvar tfl-exwm-terminal-class
  (if (executable-find "gnome-terminal")
      "Gnome-terminal"
    "Xfce4-terminal"))
(defvar tfl-exwm-terminal-command
  (if (executable-find "gnome-terminal")
      "gnome-terminal"
    "xfce4-terminal"))


;;; Functions
(defun tfl-exwm-buffer-class (buffer)
  "Return `exwm-class-name' for BUFFER."
  (with-current-buffer buffer exwm-class-name))

(defun tfl-exwm-buffer-class-p (buffer class-name)
  "Return non-nil if BUFFER `exwm-class-name' matches CLASS-NAME."
  (string-equal (tfl-exwm-buffer-class buffer) class-name))

(defun tfl-exwm-rotate-buffers-to-next (buffers target)
  "Rotate BUFFERS until its car is the buffer after TARGET.
If TARGET is not in BUFFERS, then BUFFERS is returned in its original order."
  (when buffers
    (let (prefix
          (suffix buffers))
      (while (prog1 (and (cdr suffix)
                         (not (eq (car suffix) target)))
               (setq prefix (cons (car suffix) prefix)
                     suffix (cdr suffix))))
      (if suffix
          (append suffix (reverse prefix))
        buffers))))

(defun tfl-exwm-class-buffers (target-class-name)
  "List of buffers with class TARGET-CLASS-NAME."
  (match-buffers #'tfl-exwm-buffer-class-p nil target-class-name))

(defun tfl-exwm-cycle-class (target-class-name &optional start-at-current-buffer-flag)
  "Cycle to next buffer with class TARGET-CLASS-NAME.
If START-AT-CURRENT-BUFFER-FLAG, then start at the current buffer if it has
class TARGET-CLASS-NAME."
  (let ((curr (current-buffer))
        (matching-buffers (tfl-exwm-class-buffers target-class-name)))
    (when-let (invisible-buffers (seq-filter (lambda (buffer)
                                               (or (eq buffer curr)
                                                   (not (get-buffer-window buffer))))
                                             matching-buffers))
      (let* ((sorted-buffers (sort invisible-buffers #'tfl-buffer-compare-name))
             (buffers (if (and start-at-current-buffer-flag
                               (tfl-exwm-buffer-class-p curr target-class-name))
                          (tfl-exwm-rotate-buffers-to-next sorted-buffers curr)
                        sorted-buffers)))
        (switch-to-buffer (car buffers))))))

(defun tfl-exwm-run-command (command)
  "Start COMMAND asynchronously as a disowned process whose I/O is redirected.
This allows the process to survive restarting Emacs (see `tfl-exwm-restart')."
  (interactive (list (read-shell-command "& ")))
  (start-process command nil shell-file-name "-m" "-c"
                 (concat "nohup " command "</dev/null >&/dev/null &!")))

(defun tfl-exwm-cycle-class-or-run-command (target-class-name command)
  "Cycle through buffers of class TARGET-CLASS-NAME.
If no buffers of TARGET-CLASS-NAME exist, then start COMMAND."
  (unless (tfl-exwm-cycle-class target-class-name t)
    (tfl-exwm-run-command command)))

(defun tfl-exwm-autorandr-change ()
  (tfl-exwm-run-command "autorandr -c"))

(defun tfl-exwm-switch-to-workspace-1 ()
  "Switch to workspace 1."
  (interactive)
  (exwm-workspace-switch-create 0))

(defun tfl-exwm-switch-to-workspace-2 ()
  "Switch to workspace 2."
  (interactive)
  (exwm-workspace-switch-create 1))

(defun tfl-exwm-switch-to-workspace-3 ()
  "Switch to workspace 3."
  (interactive)
  (exwm-workspace-switch-create 2))

(defun tfl-exwm-switch-to-workspace-4 ()
  "Switch to workspace 4."
  (interactive)
  (exwm-workspace-switch-create 3))

(defun tfl-exwm-switch-to-workspace-5 ()
  "Switch to workspace 5."
  (interactive)
  (exwm-workspace-switch-create 4))

(defun tfl-exwm-switch-to-workspace-6 ()
  "Switch to workspace 6."
  (interactive)
  (exwm-workspace-switch-create 5))

(defun tfl-exwm-switch-to-workspace-7 ()
  "Switch to workspace 7."
  (interactive)
  (exwm-workspace-switch-create 6))

(defun tfl-exwm-switch-to-workspace-8 ()
  "Switch to workspace 8."
  (interactive)
  (exwm-workspace-switch-create 7))

(defun tfl-exwm-switch-to-workspace-9 ()
  "Switch to workspace 9."
  (interactive)
  (exwm-workspace-switch-create 8))

(defun tfl-exwm-switch-to-workspace-0 ()
  "Switch to workspace 0."
  (interactive)
  (exwm-workspace-switch-create 9))

(defun tfl-exwm-run-screenshooter ()
  "Run flameshot in gui mode."
  (interactive)
  (tfl-exwm-run-command "flameshot gui"))

(defun tfl-exwm-switch-and-cycle-or-run (workspace-number target-class-name command)
  (when (locate-file command exec-path exec-suffixes 1)
    ;; if workspace-number is current
    (if (eq exwm-workspace-current-index workspace-number)
        ;; cycle target-class-name or run command
        (tfl-exwm-cycle-class-or-run-command target-class-name command)
      ;; otherwise switch to workspace-number
      (exwm-workspace-switch-create workspace-number)
      ;; if there are buffers of class
      (if-let (matching-buffers (tfl-exwm-class-buffers target-class-name))
          ;; only cycle if there are no visible buffers of class
          (unless (seq-filter #'get-buffer-window matching-buffers)
            (tfl-exwm-cycle-class target-class-name t))
        ;; otherwise run the command
        (delete-other-windows)
        (tfl-exwm-run-command command)))))

(defun tfl-exwm-switch-to-agenda ()
  "Switch to agenda workspace and open agenda window.
If agenda is not available, then run it."
  (interactive)
  (exwm-workspace-switch-create tfl-exmw-agenda-workspace-number)
  (delete-other-windows)
  (tfl-gtd-agenda-daily))

(defun tfl-exwm-switch-and-cycle-or-run-discord ()
  "Switch to discord workspace and cycle discord windows.
If discord is not running, the run the discord command."
  (interactive)
  (tfl-exwm-switch-and-cycle-or-run tfl-exwm-discord-workspace-number
                                    tfl-exwm-discord-class
                                    tfl-exwm-discord-command))

(defun tfl-exwm-switch-and-cycle-or-run-firefox ()
  "Switch to firefox workspace and cycle firefox windows.
If firefox is not running, the run the firefox command."
  (interactive)
  (tfl-exwm-switch-and-cycle-or-run tfl-exwm-firefox-workspace-number
                                    tfl-exwm-firefox-class
                                    tfl-exwm-firefox-command))

(defun tfl-exwm-switch-and-cycle-or-run-obsidian ()
  "Switch to obsidian workspace and cycle obsidian windows.
If obsidian is not running, the run the obsidian command."
  (interactive)
  (tfl-exwm-switch-and-cycle-or-run tfl-exwm-obsidian-workspace-number
                                    tfl-exwm-obsidian-class
                                    tfl-exwm-obsidian-command))

(defun tfl-exwm-switch-and-cycle-or-run-slack ()
  "Switch to slack workspace and cycle slack windows.
If slack is not running, the run the slack command."
  (interactive)
  (tfl-exwm-switch-and-cycle-or-run tfl-exwm-slack-workspace-number
                                    tfl-exwm-slack-class
                                    tfl-exwm-slack-command))

(defun tfl-exwm-switch-and-cycle-or-run-terminal ()
  "Switch to terminal workspace and cycle terminal windows.
If terminal is not running, the run the terminal command."
  (interactive)
  (tfl-exwm-switch-and-cycle-or-run tfl-exwm-terminal-workspace-number
                                    tfl-exwm-terminal-class
                                    tfl-exwm-terminal-command))

(defun tfl-exwm-enable ()
  "Enable my EXWM configuration."
  (interactive)
  (unless (server-running-p)
    (server-start))
  (exwm-randr-enable)
  (exwm-enable)
  (desktop-read user-emacs-directory)
  (desktop-release-lock)
  (desktop-remove))

(defun tfl-exwm-workspace-name (n)
  "Rename workspace N to align more intuitively with key bindings."
  (number-to-string (1+ n)))

(defun tfl-exwm-rename-current-buffer ()
  "Rename `current-buffer' with its `exwm-class-name'."
  (exwm-workspace-rename-buffer exwm-class-name))

(defun tfl-exwm-restart ()
  "Restart Emacs and EXWM.
Exiting with 82 ('R') signals the trampoline script to restart Emacs."
  (interactive)
  ;; EXWM is started with a trampoline script that will restart Emacs
  ;; if it returns an exit code of 82 (the ASCII code for 'R'--for
  ;; restart).  This lets me rebuild it with new packages and paths
  ;; and init scripts and restart without logging out and exiting all
  ;; my applications.
  (desktop-save user-emacs-directory t)
  (kill-emacs 82))

(provide 'tfl-exwm-core)
;;; tfl-exwm-core.el ends here
