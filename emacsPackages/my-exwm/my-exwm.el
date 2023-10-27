;;; my-exwm.el --- EXWM code and configuration -*- lexical-binding: t; -*-

;; Copyright (C) 2023 Paul Stadig

;; Version: 0.0.0

;;; Commentary:

;; None

;;; Code:

(require 'map) ; for map-apply
(require 'my)
(require 'exwm)
(require 'exwm-modeline)
(require 'exwm-config)
(require 'exwm-randr)

(defun my-exwm-buffer-class (buffer)
  (with-current-buffer buffer exwm-class-name))

(defun my-exwm-buffer-class-p (buffer class-name)
  (string-equal (my-exwm-buffer-class buffer) class-name))

(defun my-exwm-cycle-class* (target-class-name buffers prev-buffer)
  (when buffers
    (let ((buffer (car buffers))
          (buffers (cdr buffers)))
      (if (eq buffer prev-buffer)
          (my-exwm-cycle-class* target-class-name buffers nil)
        (if (not (my-exwm-buffer-class-p buffer target-class-name))
            (my-exwm-cycle-class* target-class-name buffers prev-buffer)
          (if prev-buffer
              (my-exwm-cycle-class* target-class-name buffers prev-buffer)
            (switch-to-buffer buffer)
            t))))))

(defun my-exwm-cycle-class (target-class-name &optional prev-buffer)
  (let ((buffers (sort (buffer-list) #'my-buffer-compare)))
    (or (my-exwm-cycle-class* target-class-name buffers prev-buffer)
        (my-exwm-cycle-class* target-class-name buffers nil))))

(defun my-exwm-start-disowned-process (command)
  (start-process command nil shell-file-name "-m" "-c"
                 (concat "nohup " command "</dev/null >&/dev/null &!")))

(defun my-exwm-select-or-run (target-class-name command)
  (interactive)
  (when (not (my-exwm-cycle-class target-class-name (current-buffer)))
    (my-exwm-start-disowned-process command)))

(defun my-exwm-kbd-key (m)
  (map-apply (lambda (k v) (cons (kbd k) v)) m))

(defun my-exwm-kbd-all (m)
  (map-apply (lambda (k v) (cons (kbd k) (kbd v))) m))

(defun my-exwm-make-workspace-switcher (n)
  (lambda ()
    (interactive)
    (exwm-workspace-switch-create n)))

(defun my-exwm-workspace-name (n)
  "Align workspace name more intuitively with keyboard."
  (number-to-string (mod (1+ n) 10)))

(defun my-exwm-rename-buffer ()
  (exwm-workspace-rename-buffer exwm-class-name))

(defun my-exwm-restart ()
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

(defun my-exwm-run-command-with-shell (command)
  (interactive (list (read-shell-command "$ ")))
  (my-exwm-select-or-run terminal-class terminal-command)
  (mapcar #'exwm-input--fake-key (concat "$" command "")))

(defun my-exwm-run-command (command)
  (interactive (list (read-shell-command "& ")))
  (my-exwm-start-disowned-process command))

(defun my-exwm-run-command-fun (command)
  (lambda ()
    (interactive)
    (my-exwm-start-disowned-process command)))

(defun my-exwm-autorandr-change ()
  (my-exwm-start-disowned-process "autorandr -c"))

;;;###autoload
(defun my-exwm-enable ()
  "Enable my EXWM configuration."
  (server-start)
  (enable-theme 'my-exwm)
  (exwm-randr-enable)
  (exwm-enable)
  (desktop-read user-emacs-directory)
  (desktop-release-lock)
  (desktop-remove))

;;; Configuration theme:
(deftheme my-exwm)

(custom-theme-set-variables
 'my-exwm
 '(exwm-modeline-mode t)
 `(exwm-input-global-keys
   ',(my-exwm-kbd-key
      ;; C- and M- seem to be fine as modifiers for s- but no S- since
      ;; it works inconsistently between Emacs buffers and X buffers.
      `(("s-f" . (lambda ()
                   (interactive)
                   (my-exwm-select-or-run "firefox" "firefox")))
        ("s-r" . exwm-reset)
        ("C-s-r" . my-exwm-restart)
        ("s-s" . exwm-workspace-switch)
        ("s-t" . (lambda ()
                   (interactive)
                   (my-exwm-select-or-run terminal-class terminal-command)))
        ("s-&" . my-exwm-run-command)
        ("s-$" . my-exwm-run-command-with-shell)
        ;; Align workspaces more intuitively with key bindings
        ("s-1" . ,(my-exwm-make-workspace-switcher 0))
        ("s-2" . ,(my-exwm-make-workspace-switcher 1))
        ("s-3" . ,(my-exwm-make-workspace-switcher 2))
        ("s-4" . ,(my-exwm-make-workspace-switcher 3))
        ("s-5" . ,(my-exwm-make-workspace-switcher 4))
        ("s-6" . ,(my-exwm-make-workspace-switcher 5))
        ("s-7" . ,(my-exwm-make-workspace-switcher 6))
        ("s-8" . ,(my-exwm-make-workspace-switcher 7))
        ("s-9" . ,(my-exwm-make-workspace-switcher 8))
        ("s-0" . ,(my-exwm-make-workspace-switcher 9))
        ("<XF86AudioLowerVolume>" .
         ,(my-exwm-run-command-fun "speaker-volume-lower"))
        ("<XF86AudioMicMute>" . ,(my-exwm-run-command-fun "mic-mute"))
        ("<XF86AudioMute>" . ,(my-exwm-run-command-fun "speaker-mute"))
        ("<XF86AudioRaiseVolume>" .
         ,(my-exwm-run-command-fun "speaker-volume-raise"))
        ("<XF86MonBrightnessDown>" .
         ,(my-exwm-run-command-fun "brightness-down"))
        ("<XF86MonBrightnessUp>" .
         ,(my-exwm-run-command-fun "brightness-up"))
        ("M-<XF86AudioLowerVolume>" .
         ,(my-exwm-run-command-fun "mic-volume-lower"))
        ("M-<XF86AudioMute>" . ,(my-exwm-run-command-fun "mic-mute"))
        ("M-<XF86AudioRaiseVolume>" .
         ,(my-exwm-run-command-fun "mic-volume-raise"))
        ("<print>" . ,(my-exwm-run-command-fun "flameshot gui"))
        ("C-<print>" . ,(my-exwm-run-command-fun "flameshot launcher")))))
 `(exwm-input-simulation-keys
   ',(my-exwm-kbd-all
      '(("C-_" . "C-z")
        ("C-a" . "<home>")
        ("C-b" . "<left>")
        ("C-d" . "<delete>")
        ("C-e" . "<end>")
        ("C-f" . "<right>")
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
      ,(my-exwm-kbd-all
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
      ,(my-exwm-kbd-all
        '(("C-<iso-lefttab>" . "C-z M-b")
          ("C-<tab>" . "C-z M-f")
          ("C-g" . "C-c")
          ("C-y" . "C-S-v")
          ("M-<iso-lefttab>" . "C-z C-M-B")
          ("M-<tab>" . "C-z C-M-F")
          ("s-w" . "C-z K"))))))
 '(exwm-randr-screen-change-hook 'my-exwm-autorandr-change)
 '(exwm-replace t)
 '(exwm-update-class-hook '(my-exwm-rename-buffer))
 '(exwm-workspace-index-map 'my-exwm-workspace-name)
 '(exwm-workspace-number 10)
 '(exwm-workspace-show-all-buffers t))

(provide-theme 'my-exwm)

(provide 'my-exwm)
;;; my-exwm.el ends here
