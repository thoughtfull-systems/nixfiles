;;; tfl-exwm.el --- Essentials for EXWM                                  -*- lexical-binding: t; -*-
;;
;; Copyright (C) 2024 Paul Stadig
;;
;; Author: Paul Stadig <paul@thoughtfull.systems>
;;
;;; Commentary:
;;
;; Essentials for EXWM including workspace and buffer management, key bindings, etc.
;;
;;; Code:


;;; Dependencies
(require 'desktop)
(require 'exwm)
(require 'exwm-modeline)
(require 'exwm-config)
(require 'exwm-randr)
(require 'map) ; for map-apply
(require 'tfl)


;;; Variables
(defvar terminal-class
  (if (executable-find "gnome-terminal")
      "Gnome-terminal"
    "Xfce4-terminal"))

(defvar terminal-command
  (if (executable-find "gnome-terminal")
      "gnome-terminal"
    "xfce4-terminal"))


;;; Functions
(defun tfl-exwm-buffer-class (buffer)
  (with-current-buffer buffer exwm-class-name))

(defun tfl-exwm-buffer-class-p (buffer class-name)
  (string-equal (tfl-exwm-buffer-class buffer) class-name))

(defun tfl-exwm-cycle-class* (target-class-name buffers prev-buffer)
  (when buffers
    (let ((buffer (car buffers))
          (buffers (cdr buffers)))
      (if (eq buffer prev-buffer)
          (tfl-exwm-cycle-class* target-class-name buffers nil)
        (if (not (tfl-exwm-buffer-class-p buffer target-class-name))
            (tfl-exwm-cycle-class* target-class-name buffers prev-buffer)
          (if prev-buffer
              (tfl-exwm-cycle-class* target-class-name buffers prev-buffer)
            (switch-to-buffer buffer)
            t))))))

(defun tfl-exwm-cycle-class (target-class-name &optional prev-buffer)
  (let ((buffers (sort (buffer-list) #'tfl-buffer-compare)))
    (or (tfl-exwm-cycle-class* target-class-name buffers prev-buffer)
        (tfl-exwm-cycle-class* target-class-name buffers nil))))

(defun tfl-exwm-start-disowned-process (command)
  (start-process command nil shell-file-name "-m" "-c"
                 (concat "nohup " command "</dev/null >&/dev/null &!")))

(defun tfl-exwm-select-or-run (target-class-name command)
  "Cycle through instances of TARGET-CLASS-NAME, or execute COMMAND."
  (interactive)
  (unless (tfl-exwm-cycle-class target-class-name (current-buffer))
    (tfl-exwm-start-disowned-process command)))

(defun tfl-exwm-select-or-run-in-workspace (workspace-number target-class-name command)
  "Switch to WORKSPACE-NUMBER, then cycle instances of TARGET-CLASS-NAME, or execute COMMAND."
  (interactive)
  (if (eq exwm-workspace-current-index workspace-number)
      (unless (tfl-exwm-cycle-class target-class-name (current-buffer))
        (tfl-exwm-start-disowned-process command))
    (exwm-workspace-switch workspace-number)
    (unless (tfl-exwm-buffer-class-p (current-buffer) target-class-name)
      (unless (tfl-exwm-cycle-class target-class-name nil)
        (tfl-exwm-start-disowned-process command)))
    (delete-other-windows)))

(defun tfl-exwm-kbd-key (m)
  (map-apply (lambda (k v) (cons (kbd k) v)) m))

(defun tfl-exwm-kbd-all (m)
  (map-apply (lambda (k v) (cons (kbd k) (kbd v))) m))

(defun tfl-exwm-make-workspace-switcher (n)
  (lambda ()
    (interactive)
    (exwm-workspace-switch-create n)))

(defun tfl-exwm-workspace-name (n)
  (number-to-string (mod (1+ n) 10)))

(defun tfl-exwm-rename-buffer ()
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

(defun tfl-exwm-run-command-with-shell (command)
  "Run COMMAND asynchronously with a shell in tmux."
  (interactive (list (read-shell-command "$ ")))
  (tfl-exwm-select-or-run terminal-class terminal-command)
  (mapcar #'exwm-input--fake-key (concat "$" command "")))

(defun tfl-exwm-run-command (command)
  "Run COMMAND asynchronously."
  (interactive (list (read-shell-command "& ")))
  (tfl-exwm-start-disowned-process command))

(defun tfl-exwm-run-command-fun (command)
  (lambda ()
    (interactive)
    (tfl-exwm-start-disowned-process command)))

(defun tfl-exwm-autorandr-change ()
  (tfl-exwm-start-disowned-process "autorandr -c"))

;;;###autoload
(defun tfl-exwm-enable ()
  "Enable my EXWM configuration."
  (interactive)
  (unless (server-running-p)
    (server-start))
  (unless (custom-theme-enabled-p 'tfl-exwm)
    (enable-theme 'tfl-exwm))
  (exwm-randr-enable)
  (exwm-enable)
  (desktop-read user-emacs-directory)
  (desktop-release-lock)
  (desktop-remove))


;;; Configuration
(tfl-custom-theme-set-variables
 'tfl-exwm
 '(exwm-modeline-mode t)
 `(exwm-input-global-keys
   ',(tfl-exwm-kbd-key
      ;; C- and M- seem to be fine as modifiers for s- but no S- since
      ;; it works inconsistently between Emacs buffers and X buffers.
      `(("s-f" . (lambda ()
                   (interactive)
                   (tfl-exwm-select-or-run-in-workspace 7 "firefox" "firefox")))
        ("s-r" . exwm-reset)
        ("C-s-r" . tfl-exwm-restart)
        ("s-s" . exwm-workspace-switch)
        ("s-t" . (lambda ()
                   (interactive)
                   (tfl-exwm-select-or-run-in-workspace 8 terminal-class terminal-command)))
        ("s-&" . tfl-exwm-run-command)
        ("s-$" . tfl-exwm-run-command-with-shell)
        ;; Align workspaces more intuitively with key bindings
        ("s-1" . ,(tfl-exwm-make-workspace-switcher 0))
        ("s-2" . ,(tfl-exwm-make-workspace-switcher 1))
        ("s-3" . ,(tfl-exwm-make-workspace-switcher 2))
        ("s-4" . ,(tfl-exwm-make-workspace-switcher 3))
        ("s-5" . ,(tfl-exwm-make-workspace-switcher 4))
        ("s-6" . ,(tfl-exwm-make-workspace-switcher 5))
        ("s-7" . ,(tfl-exwm-make-workspace-switcher 6))
        ("s-8" . ,(tfl-exwm-make-workspace-switcher 7))
        ("s-9" . ,(tfl-exwm-make-workspace-switcher 8))
        ("s-0" . ,(tfl-exwm-make-workspace-switcher 9))
        ("<XF86AudioLowerVolume>" .
         ,(tfl-exwm-run-command-fun "speaker-volume-lower"))
        ("<XF86AudioMicMute>" . ,(tfl-exwm-run-command-fun "mic-mute"))
        ("<XF86AudioMute>" . ,(tfl-exwm-run-command-fun "speaker-mute"))
        ("<XF86AudioRaiseVolume>" .
         ,(tfl-exwm-run-command-fun "speaker-volume-raise"))
        ("<XF86MonBrightnessDown>" .
         ,(tfl-exwm-run-command-fun "brightness-down"))
        ("<XF86MonBrightnessUp>" .
         ,(tfl-exwm-run-command-fun "brightness-up"))
        ("M-<XF86AudioLowerVolume>" .
         ,(tfl-exwm-run-command-fun "mic-volume-lower"))
        ("M-<XF86AudioMute>" . ,(tfl-exwm-run-command-fun "mic-mute"))
        ("M-<XF86AudioRaiseVolume>" .
         ,(tfl-exwm-run-command-fun "mic-volume-raise"))
        ("<print>" . ,(tfl-exwm-run-command-fun "flameshot gui"))
        ("C-<print>" . ,(tfl-exwm-run-command-fun "flameshot launcher")))))
 `(exwm-input-simulation-keys
   ',(tfl-exwm-kbd-all
      '(("C-_" . "C-z")
        ("C-a" . "<home>")
        ("C-b" . "<left>")
        ("C-d" . "<delete>")
        ("C-e" . "<end>")
        ("C-f" . "<right>")
        ("C-g" . "<escape>")
        ("C-k" . "S-<end> <delete>")
        ("C-n" . "<down>")
        ("C-p" . "<up>")
        ("C-v" . "<next>")
        ("C-w" . "C-x")
        ("C-x h" . "C-a")
        ("C-y" . "C-v")
        ("M-<" . "C-<home>")
        ("M-<backspace>" . "C-<backspace>")
        ("M->" . "C-<end>")
        ("M-b" . "C-<left>")
        ("M-f" . "C-<right>")
        ("M-v" . "<prior>")
        ("M-w" . "C-c")
        ("s-w" . "C-w"))))
 '(exwm-layout-show-all-buffers t)
 '(exwm-manage-configurations
   `(((string-equal exwm-class-name "Gnome-terminal")
      simulation-keys
      ,(tfl-exwm-kbd-all
        '(("C-<iso-lefttab>" . "C-z M-b")
          ("C-<tab>" . "C-z M-f")
          ("C-b" . "<left>")
          ("C-f" . "<right>")
          ("C-g" . "C-c")
          ("C-n" . "<down>")
          ("C-p" . "<up>")
          ("C-v" . "<next>")
          ("C-x h" . "C-a")
          ("C-y" . "C-S-v")
          ("M-<" . "<home>")
          ("M-<backspace>" . "C-<backspace>")
          ("M-<iso-lefttab>" . "C-z C-M-B")
          ("M-<tab>" . "C-z C-M-F")
          ("M->" . "<end>")
          ("M-b" . "C-<left>")
          ("M-f" . "C-<right>")
          ("M-v" . "<prior>")
          ("s-w" . "C-z K"))))
     ((string-equal exwm-class-name "Xfce4-terminal")
      simulation-keys
      ,(tfl-exwm-kbd-all
        '(("C-<iso-lefttab>" . "C-z M-b")
          ("C-<tab>" . "C-z M-f")
          ("C-g" . "C-c")
          ("C-y" . "C-S-v")
          ("M-<iso-lefttab>" . "C-z C-M-B")
          ("M-<tab>" . "C-z C-M-F")
          ("s-w" . "C-z K"))))))
 '(exwm-randr-screen-change-hook 'tfl-exwm-autorandr-change)
 '(exwm-replace t)
 '(exwm-update-class-hook '(tfl-exwm-rename-buffer))
 '(exwm-workspace-index-map 'tfl-exwm-workspace-name)
 '(exwm-workspace-number 10)
 '(exwm-workspace-show-all-buffers t))

(provide 'tfl-exwm)
;;; tfl-exwm.el ends here
