;;; tfl-gtd-process.el --- Process entries from GTD inbox                -*- lexical-binding: t; -*-

;;  Copyright (c) 2023 Technosophist

;;  Version: 0.0.0

;;; Commentary:

;; Take each entry from GTD inbox in a narrowed view for processing.

;;; Code:
(require 'tfl-gtd)
(require 'tfl-gtd-core)
(require 'org-refile)
(require 'transient)

(defvar-keymap tgp-map
  :doc "Keymap for command `tfl-gtd-process-mode', a minor mode."
  "C-c C-c" #'tgp-finalize)

(define-minor-mode tgp-mode
  "Minor mode for tfl-gtd-process."
  :lighter " tgp"
  :keymap tgp-map
  :group 'tfl-gtd-process
  (if tgp-mode
      (setq-local
       header-line-format
       `("Tasks should be SMART, singular, brief, verb-based!  "
         ,(substitute-command-keys "\\<tfl-gtd-process-map>Finish `\\[tfl-gtd-process-finalize]'.")))
    (setq-local header-line-format nil)))

(defvar tgp-buffer-name "*tfl-gtd-process*")

;;;###autoload
(defun tfl-gtd-process ()
  "Process each entry from inbox in a narrowed buffer for processing."
  (interactive)
  (if (get-buffer tgp-buffer-name)
      (pop-to-buffer tgp-buffer-name)
    (with-current-buffer
        (make-indirect-buffer (tfl-gtd-org-buffer tfl-gtd-inbox-file-name) tgp-buffer-name t)
      (tgp-mode)
      (tgp--next "No inbox items."))))

(defun tgp--next (&optional msg)
  "Move to next inbox entry for processing.
If there are no more items, show MSG or a default."
  (interactive)
  (goto-char (point-min))
  (if (not (outline-next-heading))
      (progn
        (message (or msg "No more inbox items."))
        (kill-buffer tgp-buffer-name))
    (org-fold-heading nil t)
    (org-narrow-to-subtree)
    (pop-to-buffer tgp-buffer-name)))

(defun tgp--maybe-cleanup-and-next ()
  (when (equal (buffer-name) tgp-buffer-name)
    (widen)
    (ignore-errors (kill-line))
    (tgp--next)))

(transient-define-prefix tgp-finalize ()
  [("t" "Standalone task" tgp-refile-as-task)
   ("a" "Project task for existing project or task" tgp-add-to-project)
   ("p" "New project" tgp-refile-as-project)
   ("s" "`Someday' task to do in the future" tgp-refile-as-someday)
   ("m" "`Maybe' task to think about doing" tgp-refile-as-maybe)
   ("d" "Do task now" tgp-refile-as-done)
   ("r" "Reference item" tgp-refile-as-reference)
   ("c" "Cancel item" tgp-refile-as-canceled)
   ("k" "Kill item" tgp-refile-as-killed)])

(defun tgp--validate-estimate ()
  (if (or (not (org-get-todo-state)) (org-entry-get nil "Effort"))
      t
    (message (substitute-command-keys "Missing estimate.  Set estimate with \\[org-set-effort]."))
    nil))

(defun tgp--validate-task ()
  (if (org-get-todo-state)
      t
    (message "Not a valid task.")
    nil))

(defun tgp-refile-as-task ()
  "Refile current inbox item as a standalone task."
  (interactive)
  (when (and (tgp--validate-estimate)
             (tgp--validate-task))
    (org-toggle-tag "SOMEDAY" 'off)
    (org-toggle-tag "MAYBE" 'off)
    (org-toggle-tag "REFERENCE" 'off)
    (org-refile nil nil `("Tasks" ,tfl-gtd-todo-file-name nil nil))
    (tgp--maybe-cleanup-and-next)))

(defun tgp--verify-active ()
  (if (tfl-gtd-action-p)
      t
    (org-end-of-subtree)
    nil))

(defun tgp-add-to-project ()
  "Refile current inbox item as a child of an existing project or task."
  (interactive)
  (when (tgp--validate-estimate)
    (org-toggle-tag "SOMEDAY" 'off)
    (org-toggle-tag "MAYBE" 'off)
    (org-toggle-tag "REFERENCE" 'off)
    (let ((org-refile-target-verify-function #'tgp--verify-active))
      (org-refile))
    (tgp--maybe-cleanup-and-next)))

(defun tgp--verify-project-estimates ()
  (save-excursion
    (let ((has-estimate t))
      (while (and has-estimate (outline-next-heading))
        (when (and (not (tfl-gtd-project-p))
                   (org-get-todo-state)
                   (not (org-entry-get nil "Effort")))
          (setq has-estimate nil)))
      has-estimate)))

(defun tgp--maybe-beginning-of-item ()
  (when (equal (buffer-name) tgp-buffer-name)
    (goto-char (point-min))))

(defun tgp-refile-as-project ()
  "Refile current inbox item as a new project."
  (interactive)
  (org-toggle-tag "SOMEDAY" 'off)
  (org-toggle-tag "MAYBE" 'off)
  (org-toggle-tag "REFERENCE" 'off)
  (if (not (save-excursion
             (tgp--maybe-beginning-of-item)
             (tfl-gtd-project-p)))
      (message "Not a valid project.")
    (if (not (save-excursion
               (tgp--maybe-beginning-of-item)
               (tgp--verify-project-estimates)))
        (message "Missing estimate(s).  Set estimate with %s."
                 (substitute-command-keys "\\[org-set-effort]"))
      (let ((org-refile-target-verify-function #'tgp--verify-active))
        (tgp--maybe-beginning-of-item)
        (org-refile))
      (tgp--maybe-cleanup-and-next))))

(defun tgp-refile-as-someday ()
  "Refile current inbox item as a someday task."
  (interactive)
  (org-toggle-tag "SOMEDAY" 'on)
  (org-toggle-tag "MAYBE" 'off)
  (org-toggle-tag "REFERENCE" 'off)
  (org-refile nil nil `("Someday" ,tfl-gtd-someday-file-name nil nil))
  (tgp--maybe-cleanup-and-next))

(defun tgp-refile-as-maybe ()
  "Refile current inbox item as a maybe task."
  (interactive)
  (org-toggle-tag "SOMEDAY" 'off)
  (org-toggle-tag "MAYBE" 'on)
  (org-toggle-tag "REFERENCE" 'off)
  (org-refile nil nil `("Maybe" ,tfl-gtd-maybe-file-name nil nil))
  (tgp--maybe-cleanup-and-next))

(defun tgp-refile-as-done ()
  "Completed the current inbox item and archive it."
  (interactive)
  (org-toggle-tag "SOMEDAY" 'off)
  (org-toggle-tag "MAYBE" 'off)
  (org-toggle-tag "REFERENCE" 'off)
  (org-todo "DONE")
  (org-refile nil nil `("Tasks" ,tfl-gtd-todo-file-name nil nil))
  (tgp--maybe-cleanup-and-next))

(defun tgp-refile-as-reference ()
  "Refile current inbox item as a reference item."
  (interactive)
  (org-toggle-tag "SOMEDAY" 'off)
  (org-toggle-tag "MAYBE" 'off)
  (org-toggle-tag "REFERENCE" 'on)
  (org-refile nil nil `("Reference" ,tfl-gtd-reference-file-name nil nil))
  (tgp--maybe-cleanup-and-next))

(defun tgp-refile-as-canceled ()
  "Cancel the current inbox item and archive it."
  (interactive)
  (org-toggle-tag "SOMEDAY" 'off)
  (org-toggle-tag "MAYBE" 'off)
  (org-toggle-tag "REFERENCE" 'off)
  (org-todo "CANC")
  (org-refile nil nil `("Archive" ,tfl-gtd-archive-file-name nil nil))
  (tgp--maybe-cleanup-and-next))

(defun tgp-refile-as-killed ()
  "Archive the current inbox item."
  (interactive)
  (org-toggle-tag "SOMEDAY" 'off)
  (org-toggle-tag "MAYBE" 'off)
  (org-toggle-tag "REFERENCE" 'off)
  (org-refile nil nil `("Archive" ,tfl-gtd-archive-file-name nil nil))
  (tgp--maybe-cleanup-and-next))

;; (deftheme tfl-gtd-process)
;; (custom-theme-set-variables
;;  'tfl-gtd-process
;;  )
;; (provide-theme 'tfl-gtd-process)
;; (enable-theme 'tfl-gtd-process)

(provide 'tfl-gtd-process)
;;; tfl-gtd-process.el ends here

;; Local Variables:
;; read-symbol-shorthands: (("tgp-" . "tfl-gtd-process-"))
;; End:
