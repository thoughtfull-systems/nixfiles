;;; my-gtd-process.el --- Process entries from GTD inbox  -*- lexical-binding: t; -*-

;;  Copyright (c) 2023 Paul Stadig

;;  Version: 0.0.0

;;; Commentary:

;; Take each entry from GTD inbox in a narrowed view for processing.

;;; Code:
(require 'my-gtd)
(require 'org-refile)
(require 'transient)

(defvar-keymap mgp-map
  :doc "Keymap for command `my-gtd-process-mode', a minor mode."
  "C-c C-c" #'mgp-finalize)

(define-minor-mode mgp-mode
  "Minor mode for my-gtd-process."
  :lighter " MGP"
  :keymap mgp-map
  :group 'my-gtd-process
  (if mgp-mode
      (setq-local
       header-line-format
       `("Tasks should be SMART, singular, brief, verb-based!  "
         ,(substitute-command-keys "\\<my-gtd-process-map>Finish `\\[my-gtd-process-finalize]'.")))
    (setq-local header-line-format nil)))

(defvar mgp-buffer-name "*my-gtd-process*")

;;;###autoload
(defun my-gtd-process ()
  "Process each entry from inbox in a narrowed buffer for processing."
  (interactive)
  (if (get-buffer mgp-buffer-name)
      (pop-to-buffer mgp-buffer-name)
    (with-current-buffer
        (make-indirect-buffer (my-gtd-org-buffer my-gtd-inbox-file-name) mgp-buffer-name t)
      (mgp-mode)
      (mgp--next "No inbox items."))))

(defun mgp--next (&optional msg)
  "Move to next inbox entry for processing.
If there are no more items, show MSG or a default."
  (interactive)
  (goto-char (point-min))
  (if (not (outline-next-heading))
      (progn
        (message (or msg "No more inbox items."))
        (kill-buffer mgp-buffer-name))
    (org-fold-heading nil t)
    (org-narrow-to-subtree)
    (pop-to-buffer mgp-buffer-name)))

(defun mgp--maybe-cleanup-and-next ()
  (when (equal (buffer-name) mgp-buffer-name)
    (widen)
    (ignore-errors (kill-line))
    (mgp--next)))

(transient-define-prefix mgp-finalize ()
  [("t" "Standalone task" mgp-refile-as-task)
   ("a" "Project task for existing project or task" mgp-add-to-project)
   ("p" "New project" mgp-refile-as-project)
   ("s" "`Someday' task to do in the future" mgp-refile-as-someday)
   ("m" "`Maybe' task to think about doing" mgp-refile-as-maybe)
   ("d" "Do task now" mgp-refile-as-done)
   ("r" "Reference item" mgp-refile-as-reference)
   ("c" "Cancel item" mgp-refile-as-canceled)
   ("k" "Kill item" mgp-refile-as-killed)])

(defun mgp--validate-estimate ()
  (if (org-entry-get nil "Effort")
      t
    (message (substitute-command-keys "Missing estimate.  Set estimate with \\[org-set-effort]."))
    nil))

(defun mgp-refile-as-task ()
  "Refile current inbox item as a standalone task."
  (interactive)
  (when (mgp--validate-estimate)
    (org-toggle-tag "SOMEDAY" 'off)
    (org-toggle-tag "MAYBE" 'off)
    (org-toggle-tag "REFERENCE" 'off)
    (org-refile nil nil `("Tasks" ,my-gtd-todo-file-name nil nil))
    (mgp--maybe-cleanup-and-next)))

(defun mgp--verify-active ()
  (if (my-gtd-action-p)
      t
    (org-end-of-subtree)
    nil))

(defun mgp-add-to-project ()
  "Refile current inbox item as a child of an existing project or task."
  (interactive)
  (when (mgp--validate-estimate)
    (org-toggle-tag "SOMEDAY" 'off)
    (org-toggle-tag "MAYBE" 'off)
    (org-toggle-tag "REFERENCE" 'off)
    (let ((org-refile-target-verify-function #'mgp--verify-active))
      (org-refile))
    (mgp--maybe-cleanup-and-next)))

(defun mgp--verify-project-estimates ()
  (save-excursion
    (let ((has-estimate t))
      (while (and has-estimate (outline-next-heading))
        (when (and (not (my-gtd-project-p)) (not (org-entry-get nil "Effort")))
          (setq has-estimate nil)))
      has-estimate)))

(defun mgp--maybe-beginning-of-item ()
  (when (equal (buffer-name) mgp-buffer-name)
    (goto-char (point-min))))

(defun mgp-refile-as-project ()
  "Refile current inbox item as a new project."
  (interactive)
  (org-toggle-tag "SOMEDAY" 'off)
  (org-toggle-tag "MAYBE" 'off)
  (org-toggle-tag "REFERENCE" 'off)
  (if (not (save-excursion
             (mgp--maybe-beginning-of-item)
             (my-gtd-project-p)))
      (message "Not a valid project.")
    (if (not (save-excursion
               (mgp--maybe-beginning-of-item)
               (mgp--verify-project-estimates)))
        (message "Missing estimate(s).  Set estimate with %s."
                 (substitute-command-keys "\\[org-set-effort]"))
      (let ((org-refile-target-verify-function #'mgp--verify-active))
        (mgp--maybe-beginning-of-item)
        (org-refile))
      (mgp--maybe-cleanup-and-next))))

(defun mgp-refile-as-someday ()
  "Refile current inbox item as a someday task."
  (interactive)
  (org-toggle-tag "SOMEDAY" 'on)
  (org-toggle-tag "MAYBE" 'off)
  (org-toggle-tag "REFERENCE" 'off)
  (org-refile nil nil `("Someday" ,my-gtd-someday-file-name nil nil))
  (mgp--maybe-cleanup-and-next))

(defun mgp-refile-as-maybe ()
  "Refile current inbox item as a maybe task."
  (interactive)
  (org-toggle-tag "SOMEDAY" 'off)
  (org-toggle-tag "MAYBE" 'on)
  (org-toggle-tag "REFERENCE" 'off)
  (org-refile nil nil `("Maybe" ,my-gtd-maybe-file-name nil nil))
  (mgp--maybe-cleanup-and-next))

(defun mgp-refile-as-done ()
  "Completed the current inbox item and archive it."
  (interactive)
  (org-toggle-tag "SOMEDAY" 'off)
  (org-toggle-tag "MAYBE" 'off)
  (org-toggle-tag "REFERENCE" 'off)
  (org-todo "DONE")
  (org-refile nil nil `("Tasks" ,my-gtd-todo-file-name nil nil))
  (mgp--maybe-cleanup-and-next))

(defun mgp-refile-as-reference ()
  "Refile current inbox item as a reference item."
  (interactive)
  (org-toggle-tag "SOMEDAY" 'off)
  (org-toggle-tag "MAYBE" 'off)
  (org-toggle-tag "REFERENCE" 'on)
  (org-refile nil nil `("Reference" ,my-gtd-reference-file-name nil nil))
  (mgp--maybe-cleanup-and-next))

(defun mgp-refile-as-canceled ()
  "Cancel the current inbox item and archive it."
  (interactive)
  (org-toggle-tag "SOMEDAY" 'off)
  (org-toggle-tag "MAYBE" 'off)
  (org-toggle-tag "REFERENCE" 'off)
  (org-todo "CANC")
  (org-refile nil nil `("Archive" ,my-gtd-archive-file-name nil nil))
  (mgp--maybe-cleanup-and-next))

(defun mgp-refile-as-killed ()
  "Archive the current inbox item."
  (interactive)
  (org-toggle-tag "SOMEDAY" 'off)
  (org-toggle-tag "MAYBE" 'off)
  (org-toggle-tag "REFERENCE" 'off)
  (org-refile nil nil `("Archive" ,my-gtd-archive-file-name nil nil))
  (mgp--maybe-cleanup-and-next))

;; (deftheme my-gtd-process)
;; (custom-theme-set-variables
;;  'my-gtd-process
;;  )
;; (provide-theme 'my-gtd-process)
;; (enable-theme 'my-gtd-process)

(provide 'my-gtd-process)
;;; my-gtd-process.el ends here

;; Local Variables:
;; read-symbol-shorthands: (("mgp-" . "my-gtd-process-"))
;; End:
