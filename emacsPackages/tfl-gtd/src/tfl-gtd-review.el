;;; tfl-gtd-review.el --- Weekly review                                  -*- lexical-binding: t; -*-

;;  Copyright (c) 2024 Technosophist

;;  Version: 0.0.0

;;; Commentary:

;; Weekly review agenda with waiting, someday, maybe, and stuck project sections

;;; Code:
(require 'tfl-gtd)
(require 'org-agenda)

(defvar-keymap tgr-map
  :doc "Keymap for command `tfl-gtd-review-mode', a minor mode."
  "C-c C-c" #'tgr-goto-next
  "C-c C-k" #'org-agenda-quit)

(defvar tgr--mode-header
  `("Look for entries: already DONE, I no longer want to do, I want to act on.  "
    ,(substitute-command-keys
      "\\<tfl-gtd-review-map>Next step `\\[tfl-gtd-review-goto-next]'.  ")
    ,(substitute-command-keys
      "\\<tfl-gtd-review-map>Quit `\\[org-agenda-quit]'.")))

(define-minor-mode tgr-mode
  "Minor mode for tfl-gtd-review."
  :lighter " tgr"
  :keymap tgr-map
  :group 'tfl-gtd-review
  (if tgr-mode
      (setq-local header-line-format tgr--mode-header)
    (setq-local header-line-format nil)))

(defvar tgr--buffer-name "*tfl-gtd-review*")
(defvar tgr--buffer nil)
(defvar tgr--next nil)

(defun tgr-enable-mode ()
  (when (equal (buffer-name) tgr--buffer-name)
    (tgr-mode)))

(add-hook 'org-agenda-mode-hook 'tgr-enable-mode)

(defun tgr-goto-next ()
  "Go to next step in weekly review."
  (interactive)
  (if tgr--next
      (funcall tgr--next)
    (when (buffer-live-p tgr--buffer)
      (kill-buffer tgr--buffer))))

(defun tgr--agenda ()
  "Show previous week's agenda."
  (interactive)
  (when (buffer-live-p tgr--buffer)
    (kill-buffer tgr--buffer))
  (setq tgr--mode-header
        `("Look for followup tasks.  "
          ,(substitute-command-keys
            "\\<tfl-gtd-review-map>Next step `\\[tfl-gtd-review-goto-next]'.  ")
          ,(substitute-command-keys
            "\\<tfl-gtd-review-map>Quit `\\[org-agenda-quit]'.")))
  (let* ((org-agenda-custom-commands
          `(("z" "Weekly review"
             ((agenda ""))
             ((org-agenda-prefix-format
               '((agenda . " %i %?-12t% s%b")
                 (todo . " %i %b")
                 (tags . " %i %b")
                 (search . " %i %b")))
              (org-agenda-tag-filter-preset
               '("-ARCHIVE" "-DONE" "-CANC"))
              (org-agenda-buffer-name
               ,tgr--buffer-name)
              (org-agenda-window-setup 'current-window))))))
    (org-agenda nil "z")
    (setq tgr--buffer (current-buffer))
    (setq tgr--next #'tgr--wait)))

(defun tgr--wait ()
  "Show agenda with waiting tasks."
  (interactive)
  (when (buffer-live-p tgr--buffer)
    (kill-buffer tgr--buffer))
  (setq tgr--mode-header
        `("Look for entries: already DONE, I no longer want to do, I want to act on.  "
          ,(substitute-command-keys
            "\\<tfl-gtd-review-map>Next step `\\[tfl-gtd-review-goto-next]'.  ")
          ,(substitute-command-keys
            "\\<tfl-gtd-review-map>Quit `\\[org-agenda-quit]'.")))
  (let* ((org-agenda-custom-commands
          `(("z" "Weekly review"
             ((tags "WAIT" ((org-agenda-overriding-header "Waiting tasks"))))
             ((org-agenda-prefix-format
               '((agenda . " %i %?-12t% s%b")
                 (todo . " %i %b")
                 (tags . " %i %b")
                 (search . " %i %b")))
              (org-agenda-tag-filter-preset
               '("-ARCHIVE" "-DONE" "-CANC"))
              (org-agenda-buffer-name
               ,tgr--buffer-name)
              (org-agenda-window-setup 'current-window))))))
    (org-agenda nil "z")
    (setq tgr--buffer (current-buffer))
    (setq tgr--next #'tgr--someday)))

(defun tgr--someday ()
  "Show agenda with someday tasks."
  (interactive)
  (when (buffer-live-p tgr--buffer)
    (kill-buffer tgr--buffer))
  (setq tgr--mode-header
        `("Look for entries: already DONE, I no longer want to do, I want to act on.  "
          ,(substitute-command-keys
            "\\<tfl-gtd-review-map>Next step `\\[tfl-gtd-review-goto-next]'.  ")
          ,(substitute-command-keys
            "\\<tfl-gtd-review-map>Quit `\\[org-agenda-quit]'.")))
  (let* ((org-agenda-custom-commands
          `(("z" "Weekly review"
             ((tags "SOMEDAY" ((org-agenda-overriding-header "Someday tasks"))))
             ((org-agenda-prefix-format
               '((agenda . " %i %?-12t% s%b")
                 (todo . " %i %b")
                 (tags . " %i %b")
                 (search . " %i %b")))
              (org-agenda-tag-filter-preset
               '("-ARCHIVE" "-DONE" "-CANC"))
              (org-agenda-buffer-name
               ,tgr--buffer-name)
              (org-agenda-window-setup 'current-window))))))
    (org-agenda nil "z")
    (setq tgr--buffer (current-buffer))
    (setq tgr--next #'tgr--maybe)))

(defun tgr--maybe ()
  "Show agenda with maybe tasks."
  (interactive)
  (when (buffer-live-p tgr--buffer)
    (kill-buffer tgr--buffer))
  (setq tgr--mode-header
        `("Look for entries: already DONE, I no longer want to do, I want to act on.  "
          ,(substitute-command-keys
            "\\<tfl-gtd-review-map>Next step `\\[tfl-gtd-review-goto-next]'.  ")
          ,(substitute-command-keys
            "\\<tfl-gtd-review-map>Quit `\\[org-agenda-quit]'.")))
  (let* ((org-agenda-custom-commands
          `(("z" "Weekly review"
             ((tags "MAYBE" ((org-agenda-overriding-header "Maybe tasks"))))
             ((org-agenda-prefix-format
               '((agenda . " %i %?-12t% s%b")
                 (todo . " %i %b")
                 (tags . " %i %b")
                 (search . " %i %b")))
              (org-agenda-tag-filter-preset
               '("-ARCHIVE" "-DONE" "-CANC"))
              (org-agenda-buffer-name
               ,tgr--buffer-name)
              (org-agenda-window-setup 'current-window))))))
    (org-agenda nil "z")
    (setq tgr--buffer (current-buffer))
    (setq tgr--next nil)))

;;;###autoload
(defun tfl-gtd-review ()
  "Weekly review agenda with waiting, someday, maybe, and stuck project sections."
  (interactive)
  (if (buffer-live-p tgr--buffer)
      (switch-to-buffer tgr--buffer)
    (tgr--agenda)))

(provide 'tfl-gtd-review)
;;; tfl-gtd-review.el ends here

;; Local Variables:
;; read-symbol-shorthands: (("tgr-" . "tfl-gtd-review-"))
;; End:
