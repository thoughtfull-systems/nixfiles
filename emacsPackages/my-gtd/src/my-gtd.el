;;; my-gtd.el --- Project and task management using a GTDish process  -*- lexical-binding: t; -*-

;;  Copyright (c) 2023 Paul Stadig

;;  Version: 0.0.0

;;; Commentary:

;; Project and task management using a GTDish process

;;; Code:

(require 'org)

(defun my-gtd--set-agenda-file (sym val)
  (let ((old-file (ignore-errors (file-truename (concat org-directory "/" (symbol-value sym)))))
        (new-file (file-truename (concat org-directory "/" val))))
    (set-default-toplevel-value sym val)
    (org-store-new-agenda-file-list (cons new-file (remove old-file org-agenda-files)))))

(defgroup my-gtd nil
  "Custom variables for my-gtd."
  :group 'org)

(defcustom my-gtd-inbox-file-name "in.org"
  "File containing inbox items.
This is where `org-capture' items should go.  Inbox is processed using
`my-gtd-process-inbox'.

File is relative to `org-directory'.  Added to `org-agenda-files' when set."
  :group 'my-gtd
  :type 'string
  :set #'my-gtd--set-agenda-file)

(defcustom my-gtd-todo-file-name "todo.org"
  "File containing projects and tasks.
File is relative to `org-directory'.  Added to `org-agenda-files' when set."
  :group 'my-gtd
  :type 'string
  :set #'my-gtd--set-agenda-file)

(defcustom my-gtd-habits-file-name "habits.org"
  "File containing habits.
File is relative to `org-directory'.  Added to `org-agenda-files' when set."
  :group 'my-gtd
  :type 'string
  :set #'my-gtd--set-agenda-file)

(defcustom my-gtd-someday-file-name "someday.org"
  "File containing someday tasks.
Someday tasks are actionable tasks to do in the future.

File is relative to `org-directory'.  Added to `org-agenda-files' when set."
  :group 'my-gtd
  :type 'string
  :set #'my-gtd--set-agenda-file)

(defcustom my-gtd-maybe-file-name "maybe.org"
  "File containing maybe tasks.
Maybe tasks are actionable tasks that may not be done.

File is relative to `org-directory'.  Added to `org-agenda-files' when set."
  :group 'my-gtd
  :type 'string
  :set #'my-gtd--set-agenda-file)

(defcustom my-gtd-reference-file-name "reference.org"
  "File containing reference items.
Reference items are non-actionable items to keep for reference.

File is relative to `org-directory'."
  :group 'my-gtd
  :type 'string)

(defcustom my-gtd-archive-file-name "archive.org"
  "File containing archived tasks.
File is relative to `org-directory'.  Added to `org-agenda-files' when set."
  :group 'my-gtd
  :type 'string
  :set #'my-gtd--set-agenda-file)

(defun my-gtd-org-file (name)
  (concat org-directory "/" name))

(defun my-gtd-org-buffer (file)
  (let ((file (my-gtd-org-file file)))
    (or (find-buffer-visiting file)
        (find-file-noselect file))))

(defvar my-gtd--inactive-todo-states
  '("WAIT" "CANC" "DONE"))

(defvar my-gtd--inactive-tags
  '("WAIT" "HABIT" "SOMEDAY" "MAYBE" "CANC" "DONE" "ARCHIVE" "REFERENCE"))

(defun my-gtd-active-p ()
  (and (not (seq-intersection my-gtd--inactive-todo-states (list (org-get-todo-state))))
       (not (seq-intersection my-gtd--inactive-tags (org-get-tags)))))

(defun my-gtd-action-p ()
  (and (my-gtd-active-p)
       (not (equal "habit" (org-entry-get nil "STYLE")))
       (not (org-get-deadline-time nil))
       (not (org-get-scheduled-time nil))))

(defvar-local my-gtd--task-regexp nil)

(defun my-gtd-set-regexp ()
  (setq-local my-gtd--task-regexp (concat "^\\(\\*+\\)\\(?: +" org-todo-regexp "\\)")))

(defun my-gtd-project-p ()
  ;; top-level non-todo (top-level TODOs are singleton tasks)
  (or (and (= 1 (org-outline-level)) (not (org-get-todo-state)))
      ;; any-level TODO with a task child
      (let ((eos (save-excursion (org-end-of-subtree) (point))))
        (save-excursion
          (outline-end-of-heading)
          (search-forward-regexp my-gtd--task-regexp eos t)))))

(defvar org-state)
(defvar org-last-state)

(defun my-gtd-add-delegatee-to-waiting ()
  (if (equal org-state "WAIT")
      (let ((delegatee (org-read-property-value "DELEGATEE")))
        (org-set-property "DELEGATEE" delegatee)
        (org-add-log-setup 'state org-state org-last-state nil (concat "Delegated to " delegatee)))
    (when (org-entry-get nil "DELEGATEE")
      (org-delete-property "DELEGATEE"))))

(defun my-gtd-add-created-to-todo ()
  (when org-state
    (when (not (org-entry-get nil "CREATED"))
      (org-set-property "CREATED"
                        (format-time-string (org-time-stamp-format t t) (current-time))))))

(deftheme my-gtd)
(custom-theme-set-variables
 'my-gtd
 '(org-after-todo-state-change-hook '(my-gtd-add-delegatee-to-waiting my-gtd-add-created-to-todo))
 '(org-log-done 'time)
 '(org-log-into-drawer t)
 '(org-log-states-order-reversed nil)
 '(org-todo-keyword-faces '(("WAIT" . "blue") ("CANC" . "orange")))
 '(org-todo-keywords
   '((sequence "TODO(t!)" "WAIT(w@/!)" "|" "CANC(c@)" "DONE(d)")))
 '(org-todo-state-tags-triggers
   '(("TODO"
      ("WAIT")
      ("CANC")
      ("DONE"))
     ("WAIT"
      ("WAIT" . t)
      ("HABIT")
      ("SOMEDAY")
      ("MAYBE")
      ("CANC")
      ("DONE"))
     ("CANC"
      ("WAIT")
      ("HABIT")
      ("SOMEDAY")
      ("MAYBE")
      ("CANC" . t)
      ("DONE"))
     ("DONE"
      ("WAIT")
      ("SOMEDAY")
      ("MAYBE")
      ("CANC")
      ("DONE" . t)
      ())
     (""
      ("WAIT")
      ("HABIT")
      ("SOMEDAY")
      ("MAYBE")
      ("CANC")
      ("DONE")))))
(provide-theme 'my-gtd)
(enable-theme 'my-gtd)

(provide 'my-gtd)
;;; my-gtd.el ends here
