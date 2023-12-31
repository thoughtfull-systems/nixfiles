;;; pjs-exwm.el --- EXWM code and configuration                          -*- lexical-binding: t; -*-

;; Copyright (C) 2023 Paul Stadig

;; Version: 0.0.0

;;; Commentary:

;; None

;;; Code:

(require 'desktop)
(require 'map) ; for map-apply
(require 'pjs)
(require 'exwm)
(require 'exwm-modeline)
(require 'exwm-config)
(require 'exwm-randr)

(defun pjs-exwm-buffer-class (buffer)
  (with-current-buffer buffer exwm-class-name))

(defun pjs-exwm-buffer-class-p (buffer class-name)
  (string-equal (pjs-exwm-buffer-class buffer) class-name))

(defun pjs-exwm-cycle-class* (target-class-name buffers prev-buffer)
  (when buffers
    (let ((buffer (car buffers))
          (buffers (cdr buffers)))
      (if (eq buffer prev-buffer)
          (pjs-exwm-cycle-class* target-class-name buffers nil)
        (if (not (pjs-exwm-buffer-class-p buffer target-class-name))
            (pjs-exwm-cycle-class* target-class-name buffers prev-buffer)
          (if prev-buffer
              (pjs-exwm-cycle-class* target-class-name buffers prev-buffer)
            (switch-to-buffer buffer)
            t))))))

(defun pjs-exwm-cycle-class (target-class-name &optional prev-buffer)
  (let ((buffers (sort (buffer-list) #'pjs-buffer-compare)))
    (or (pjs-exwm-cycle-class* target-class-name buffers prev-buffer)
        (pjs-exwm-cycle-class* target-class-name buffers nil))))

(defun pjs-exwm-start-disowned-process (command)
  (start-process command nil shell-file-name "-m" "-c"
                 (concat "nohup " command "</dev/null >&/dev/null &!")))

(defun pjs-exwm-select-or-run (target-class-name command)
  "Cycle through instances of TARGET-CLASS-NAME, or execute COMMAND."
  (interactive)
  (unless (pjs-exwm-cycle-class target-class-name (current-buffer))
    (pjs-exwm-start-disowned-process command)))

(defun pjs-exwm-kbd-key (m)
  (map-apply (lambda (k v) (cons (kbd k) v)) m))

(defun pjs-exwm-kbd-all (m)
  (map-apply (lambda (k v) (cons (kbd k) (kbd v))) m))

(defun pjs-exwm-make-workspace-switcher (n)
  (lambda ()
    (interactive)
    (exwm-workspace-switch-create n)))

(defun pjs-exwm-workspace-name (n)
  (number-to-string (mod (1+ n) 10)))

(defun pjs-exwm-rename-buffer ()
  (exwm-workspace-rename-buffer exwm-class-name))

(defun pjs-exwm-restart ()
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

(defvar terminal-class
  (if (executable-find "gnome-terminal")
      "Gnome-terminal"
    "Xfce4-terminal"))

(defvar terminal-command
  (if (executable-find "gnome-terminal")
      "gnome-terminal"
    "xfce4-terminal"))

(defun pjs-exwm-run-command-with-shell (command)
  "Run COMMAND asynchronously with a shell in tmux."
  (interactive (list (read-shell-command "$ ")))
  (pjs-exwm-select-or-run terminal-class terminal-command)
  (mapcar #'exwm-input--fake-key (concat "$" command "")))

(defun pjs-exwm-run-command (command)
  "Run COMMAND asynchronously."
  (interactive (list (read-shell-command "& ")))
  (pjs-exwm-start-disowned-process command))

(defun pjs-exwm-run-command-fun (command)
  (lambda ()
    (interactive)
    (pjs-exwm-start-disowned-process command)))

(defun pjs-exwm-autorandr-change ()
  (pjs-exwm-start-disowned-process "autorandr -c"))

;;;###autoload
(defun pjs-exwm-enable ()
  "Enable my EXWM configuration."
  (interactive)
  (unless (server-running-p)
    (server-start))
  (unless (custom-theme-enabled-p 'pjs-exwm)
    (enable-theme 'pjs-exwm))
  (exwm-randr-enable)
  (exwm-enable)
  (desktop-read user-emacs-directory)
  (desktop-release-lock)
  (desktop-remove))

(deftheme pjs-exwm)
(custom-theme-set-variables
 'pjs-exwm
 '(exwm-modeline-mode t)
 `(exwm-input-global-keys
   ',(pjs-exwm-kbd-key
      ;; C- and M- seem to be fine as modifiers for s- but no S- since
      ;; it works inconsistently between Emacs buffers and X buffers.
      `(("s-f" . (lambda ()
                   (interactive)
                   (pjs-exwm-select-or-run "firefox" "firefox")))
        ("s-r" . exwm-reset)
        ("C-s-r" . pjs-exwm-restart)
        ("s-s" . exwm-workspace-switch)
        ("s-t" . (lambda ()
                   (interactive)
                   (pjs-exwm-select-or-run terminal-class terminal-command)))
        ("s-&" . pjs-exwm-run-command)
        ("s-$" . pjs-exwm-run-command-with-shell)
        ;; Align workspaces more intuitively with key bindings
        ("s-1" . ,(pjs-exwm-make-workspace-switcher 0))
        ("s-2" . ,(pjs-exwm-make-workspace-switcher 1))
        ("s-3" . ,(pjs-exwm-make-workspace-switcher 2))
        ("s-4" . ,(pjs-exwm-make-workspace-switcher 3))
        ("s-5" . ,(pjs-exwm-make-workspace-switcher 4))
        ("s-6" . ,(pjs-exwm-make-workspace-switcher 5))
        ("s-7" . ,(pjs-exwm-make-workspace-switcher 6))
        ("s-8" . ,(pjs-exwm-make-workspace-switcher 7))
        ("s-9" . ,(pjs-exwm-make-workspace-switcher 8))
        ("s-0" . ,(pjs-exwm-make-workspace-switcher 9))
        ("<XF86AudioLowerVolume>" .
         ,(pjs-exwm-run-command-fun "speaker-volume-lower"))
        ("<XF86AudioMicMute>" . ,(pjs-exwm-run-command-fun "mic-mute"))
        ("<XF86AudioMute>" . ,(pjs-exwm-run-command-fun "speaker-mute"))
        ("<XF86AudioRaiseVolume>" .
         ,(pjs-exwm-run-command-fun "speaker-volume-raise"))
        ("<XF86MonBrightnessDown>" .
         ,(pjs-exwm-run-command-fun "brightness-down"))
        ("<XF86MonBrightnessUp>" .
         ,(pjs-exwm-run-command-fun "brightness-up"))
        ("M-<XF86AudioLowerVolume>" .
         ,(pjs-exwm-run-command-fun "mic-volume-lower"))
        ("M-<XF86AudioMute>" . ,(pjs-exwm-run-command-fun "mic-mute"))
        ("M-<XF86AudioRaiseVolume>" .
         ,(pjs-exwm-run-command-fun "mic-volume-raise"))
        ("<print>" . ,(pjs-exwm-run-command-fun "flameshot gui"))
        ("C-<print>" . ,(pjs-exwm-run-command-fun "flameshot launcher")))))
 `(exwm-input-simulation-keys
   ',(pjs-exwm-kbd-all
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
      ,(pjs-exwm-kbd-all
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
      ,(pjs-exwm-kbd-all
        '(("C-<iso-lefttab>" . "C-z M-b")
          ("C-<tab>" . "C-z M-f")
          ("C-g" . "C-c")
          ("C-y" . "C-S-v")
          ("M-<iso-lefttab>" . "C-z C-M-B")
          ("M-<tab>" . "C-z C-M-F")
          ("s-w" . "C-z K"))))))
 '(exwm-randr-screen-change-hook 'pjs-exwm-autorandr-change)
 '(exwm-replace t)
 '(exwm-update-class-hook '(pjs-exwm-rename-buffer))
 '(exwm-workspace-index-map 'pjs-exwm-workspace-name)
 '(exwm-workspace-number 10)
 '(exwm-workspace-show-all-buffers t))
(provide-theme 'pjs-exwm)

(provide 'pjs-exwm)
;;; pjs-exwm.el ends here
