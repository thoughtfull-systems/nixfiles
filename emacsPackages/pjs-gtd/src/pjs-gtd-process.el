;;; pjs-gtd-process.el --- Process entries from GTD inbox  -*- lexical-binding: t; -*-

;;  Copyright (c) 2023 Paul Stadig

;;  Version: 0.0.0

;;; Commentary:

;; Take each entry from GTD inbox in a narrowed view for processing.

;;; Code:
(require 'pjs-gtd)
(require 'org-refile)
(require 'transient)

(defvar-keymap pgp-map
  :doc "Keymap for command `pjs-gtd-process-mode', a minor mode."
  "C-c C-c" #'pgp-finalize)

(define-minor-mode pgp-mode
  "Minor mode for pjs-gtd-process."
  :lighter " PGP"
  :keymap pgp-map
  :group 'pjs-gtd-process
  (if pgp-mode
      (setq-local
       header-line-format
       `("Tasks should be SMART, singular, brief, verb-based!  "
         ,(substitute-command-keys "\\<pjs-gtd-process-map>Finish `\\[pjs-gtd-process-finalize]'.")))
    (setq-local header-line-format nil)))

(defvar pgp-buffer-name "*pjs-gtd-process*")

;;;###autoload
(defun pjs-gtd-process ()
  "Process each entry from inbox in a narrowed buffer for processing."
  (interactive)
  (if (get-buffer pgp-buffer-name)
      (pop-to-buffer pgp-buffer-name)
    (with-current-buffer
        (make-indirect-buffer (pjs-gtd-org-buffer pjs-gtd-inbox-file-name) pgp-buffer-name t)
      (pgp-mode)
      (pgp--next "No inbox items."))))

(defun pgp--next (&optional msg)
  "Move to next inbox entry for processing.
If there are no more items, show MSG or a default."
  (interactive)
  (goto-char (point-min))
  (if (not (outline-next-heading))
      (progn
        (message (or msg "No more inbox items."))
        (kill-buffer pgp-buffer-name))
    (org-fold-heading nil t)
    (org-narrow-to-subtree)
    (pop-to-buffer pgp-buffer-name)))

(defun pgp--maybe-cleanup-and-next ()
  (when (equal (buffer-name) pgp-buffer-name)
    (widen)
    (ignore-errors (kill-line))
    (pgp--next)))

(transient-define-prefix pgp-finalize ()
  [("t" "Standalone task" pgp-refile-as-task)
   ("a" "Project task for existing project or task" pgp-add-to-project)
   ("p" "New project" pgp-refile-as-project)
   ("s" "`Someday' task to do in the future" pgp-refile-as-someday)
   ("m" "`Maybe' task to think about doing" pgp-refile-as-maybe)
   ("d" "Do task now" pgp-refile-as-done)
   ("r" "Reference item" pgp-refile-as-reference)
   ("c" "Cancel item" pgp-refile-as-canceled)
   ("k" "Kill item" pgp-refile-as-killed)])

(defun pgp--validate-estimate ()
  (if (or (not (org-get-todo-state)) (org-entry-get nil "Effort"))
      t
    (message (substitute-command-keys "Missing estimate.  Set estimate with \\[org-set-effort]."))
    nil))

(defun pgp--validate-task ()
  (if (org-get-todo-state)
      t
    (message "Not a valid task.")
    nil))

(defun pgp-refile-as-task ()
  "Refile current inbox item as a standalone task."
  (interactive)
  (when (and (pgp--validate-estimate)
             (pgp--validate-task))
    (org-toggle-tag "SOMEDAY" 'off)
    (org-toggle-tag "MAYBE" 'off)
    (org-toggle-tag "REFERENCE" 'off)
    (org-refile nil nil `("Tasks" ,pjs-gtd-todo-file-name nil nil))
    (pgp--maybe-cleanup-and-next)))

(defun pgp--verify-active ()
  (if (pjs-gtd-action-p)
      t
    (org-end-of-subtree)
    nil))

(defun pgp-add-to-project ()
  "Refile current inbox item as a child of an existing project or task."
  (interactive)
  (when (pgp--validate-estimate)
    (org-toggle-tag "SOMEDAY" 'off)
    (org-toggle-tag "MAYBE" 'off)
    (org-toggle-tag "REFERENCE" 'off)
    (let ((org-refile-target-verify-function #'pgp--verify-active))
      (org-refile))
    (pgp--maybe-cleanup-and-next)))

(defun pgp--verify-project-estimates ()
  (save-excursion
    (let ((has-estimate t))
      (while (and has-estimate (outline-next-heading))
        (when (and (not (pjs-gtd-project-p))
                   (org-get-todo-state)
                   (not (org-entry-get nil "Effort")))
          (setq has-estimate nil)))
      has-estimate)))

(defun pgp--maybe-beginning-of-item ()
  (when (equal (buffer-name) pgp-buffer-name)
    (goto-char (point-min))))

(defun pgp-refile-as-project ()
  "Refile current inbox item as a new project."
  (interactive)
  (org-toggle-tag "SOMEDAY" 'off)
  (org-toggle-tag "MAYBE" 'off)
  (org-toggle-tag "REFERENCE" 'off)
  (if (not (save-excursion
             (pgp--maybe-beginning-of-item)
             (pjs-gtd-project-p)))
      (message "Not a valid project.")
    (if (not (save-excursion
               (pgp--maybe-beginning-of-item)
               (pgp--verify-project-estimates)))
        (message "Missing estimate(s).  Set estimate with %s."
                 (substitute-command-keys "\\[org-set-effort]"))
      (let ((org-refile-target-verify-function #'pgp--verify-active))
        (pgp--maybe-beginning-of-item)
        (org-refile))
      (pgp--maybe-cleanup-and-next))))

(defun pgp-refile-as-someday ()
  "Refile current inbox item as a someday task."
  (interactive)
  (org-toggle-tag "SOMEDAY" 'on)
  (org-toggle-tag "MAYBE" 'off)
  (org-toggle-tag "REFERENCE" 'off)
  (org-refile nil nil `("Someday" ,pjs-gtd-someday-file-name nil nil))
  (pgp--maybe-cleanup-and-next))

(defun pgp-refile-as-maybe ()
  "Refile current inbox item as a maybe task."
  (interactive)
  (org-toggle-tag "SOMEDAY" 'off)
  (org-toggle-tag "MAYBE" 'on)
  (org-toggle-tag "REFERENCE" 'off)
  (org-refile nil nil `("Maybe" ,pjs-gtd-maybe-file-name nil nil))
  (pgp--maybe-cleanup-and-next))

(defun pgp-refile-as-done ()
  "Completed the current inbox item and archive it."
  (interactive)
  (org-toggle-tag "SOMEDAY" 'off)
  (org-toggle-tag "MAYBE" 'off)
  (org-toggle-tag "REFERENCE" 'off)
  (org-todo "DONE")
  (org-refile nil nil `("Tasks" ,pjs-gtd-todo-file-name nil nil))
  (pgp--maybe-cleanup-and-next))

(defun pgp-refile-as-reference ()
  "Refile current inbox item as a reference item."
  (interactive)
  (org-toggle-tag "SOMEDAY" 'off)
  (org-toggle-tag "MAYBE" 'off)
  (org-toggle-tag "REFERENCE" 'on)
  (org-refile nil nil `("Reference" ,pjs-gtd-reference-file-name nil nil))
  (pgp--maybe-cleanup-and-next))

(defun pgp-refile-as-canceled ()
  "Cancel the current inbox item and archive it."
  (interactive)
  (org-toggle-tag "SOMEDAY" 'off)
  (org-toggle-tag "MAYBE" 'off)
  (org-toggle-tag "REFERENCE" 'off)
  (org-todo "CANC")
  (org-refile nil nil `("Archive" ,pjs-gtd-archive-file-name nil nil))
  (pgp--maybe-cleanup-and-next))

(defun pgp-refile-as-killed ()
  "Archive the current inbox item."
  (interactive)
  (org-toggle-tag "SOMEDAY" 'off)
  (org-toggle-tag "MAYBE" 'off)
  (org-toggle-tag "REFERENCE" 'off)
  (org-refile nil nil `("Archive" ,pjs-gtd-archive-file-name nil nil))
  (pgp--maybe-cleanup-and-next))

;; (deftheme pjs-gtd-process)
;; (custom-theme-set-variables
;;  'pjs-gtd-process
;;  )
;; (provide-theme 'pjs-gtd-process)
;; (enable-theme 'pjs-gtd-process)

(provide 'pjs-gtd-process)
;;; pjs-gtd-process.el ends here

;; Local Variables:
;; read-symbol-shorthands: (("pgp-" . "pjs-gtd-process-"))
;; End:
