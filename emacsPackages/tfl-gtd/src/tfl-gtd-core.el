;;; tfl-gtd-core.el --- Project and task management with GTDish process  -*- lexical-binding: t; -*-
;;
;;  Copyright (c) 2024 Technosophist
;;
;;  Version: 0.0.0
;;
;;; Commentary:
;;
;; Project and task management with GTDish process.
;;
;;; Code:

(require 'org)
(require 'org-capture)

(defun tfl-gtd--set-agenda-file (sym val)
  (let* ((new-file (file-truename (concat org-directory "/" val)))
         (old-file (file-truename (concat org-directory "/" (or (ignore-errors (symbol-value sym))
                                                                (custom--standard-value sym))))))
    (set-default-toplevel-value sym val)
    (org-store-new-agenda-file-list (cons new-file (remove old-file org-agenda-files)))))

(defgroup tfl-gtd nil
  "Custom variables for tfl-gtd."
  :group 'org)

(defcustom tfl-gtd-inbox-file-name "in.org"
  "File containing inbox items.
This is where `org-capture' items should go.  Inbox is processed using
`tfl-gtd-process-inbox'.

File is relative to `org-directory'.  Added to `org-agenda-files' when set."
  :group 'tfl-gtd
  :type 'string)

(defcustom tfl-gtd-todo-file-name "todo.org"
  "File containing projects and tasks.
File is relative to `org-directory'.  Added to `org-agenda-files' when set."
  :group 'tfl-gtd
  :type 'string
  :set #'tfl-gtd--set-agenda-file)

(defcustom tfl-gtd-habits-file-name "habits.org"
  "File containing habits.
File is relative to `org-directory'.  Added to `org-agenda-files' when set."
  :group 'tfl-gtd
  :type 'string
  :set #'tfl-gtd--set-agenda-file)

(defcustom tfl-gtd-someday-file-name "someday.org"
  "File containing someday tasks.
Someday tasks are actionable tasks to do in the future.

File is relative to `org-directory'.  Added to `org-agenda-files' when set."
  :group 'tfl-gtd
  :type 'string
  :set #'tfl-gtd--set-agenda-file)

(defcustom tfl-gtd-maybe-file-name "maybe.org"
  "File containing maybe tasks.
Maybe tasks are actionable tasks that may not be done.

File is relative to `org-directory'.  Added to `org-agenda-files' when set."
  :group 'tfl-gtd
  :type 'string
  :set #'tfl-gtd--set-agenda-file)

(defcustom tfl-gtd-reference-file-name "reference.org"
  "File containing reference items.
Reference items are non-actionable items to keep for reference.

File is relative to `org-directory'."
  :group 'tfl-gtd
  :type 'string)

(defcustom tfl-gtd-archive-file-name "archive.org"
  "File containing archived tasks.
File is relative to `org-directory'.  Added to `org-agenda-files' when set."
  :group 'tfl-gtd
  :type 'string
  :set #'tfl-gtd--set-agenda-file)

(defun tfl-gtd-org-file (name)
  (concat org-directory "/" name))

(defun tfl-gtd-org-buffer (file)
  (let ((file (tfl-gtd-org-file file)))
    (or (find-buffer-visiting file)
        (find-file-noselect file))))

(defvar tfl-gtd--inactive-todo-states
  '("WAIT" "CANC" "DONE"))

(defvar tfl-gtd--inactive-tags
  '("WAIT" "HABIT" "SOMEDAY" "TOLISTEN" "TOREAD" "TOWATCH" "MAYBE" "CANC" "DONE" "ARCHIVE"
    "REFERENCE"))

(defun tfl-gtd-active-p ()
  (and (not (seq-intersection tfl-gtd--inactive-todo-states (list (org-get-todo-state))))
       (not (seq-intersection tfl-gtd--inactive-tags (org-get-tags)))))

(defun tfl-gtd-action-p ()
  (and (tfl-gtd-active-p)
       (not (equal "habit" (org-entry-get nil "STYLE")))
       (not (org-get-deadline-time nil))
       (not (org-get-scheduled-time nil))))

(defvar-local tfl-gtd--task-regexp nil)

(defun tfl-gtd-set-regexp ()
  (setq-local tfl-gtd--task-regexp (concat "^\\(\\*+\\)\\(?: +" org-todo-regexp "\\)")))

(defun tfl-gtd-project-p ()
  ;; top-level non-todo (top-level TODOs are singleton tasks)
  (or (and (= 1 (org-outline-level)) (not (org-get-todo-state)))
      ;; any-level TODO with a task child
      (let ((eos (save-excursion (org-end-of-subtree) (point))))
        (save-excursion
          (outline-end-of-heading)
          (search-forward-regexp tfl-gtd--task-regexp eos t)))))

(defvar org-state)
(defvar org-last-state)

(defun tfl-gtd-add-delegatee-to-waiting ()
  (if (equal org-state "WAIT")
      (let ((delegatee (org-read-property-value "DELEGATEE")))
        (org-set-property "DELEGATEE" delegatee)
        (org-add-log-setup 'state org-state org-last-state nil (concat "Delegated to " delegatee)))
    (when (org-entry-get nil "DELEGATEE")
      (org-delete-property "DELEGATEE"))))

(defun tfl-gtd-add-created-to-todo ()
  (when org-state
    (unless (org-entry-get nil "CREATED")
      (org-set-property "CREATED"
                        (format-time-string (org-time-stamp-format t t) (current-time))))))

(defun tfl-gtd-capture ()
  "Quickly capture a TODO item."
  (interactive)
  (let ((org-capture-templates
         '(("z" "Todo" entry
            (file "")
            "* TODO %?\12:PROPERTIES:\12:CREATED: %U\12:END:"))))
    (org-capture nil "z")))

(provide 'tfl-gtd-core)
;;; tfl-gtd-core.el ends here
