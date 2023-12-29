;;; pjs-gtd.el --- Project and task management using a GTDish process  -*- lexical-binding: t; -*-

;;  Copyright (c) 2023 Paul Stadig

;;  Version: 0.0.0

;;; Commentary:

;; Project and task management using a GTDish process

;;; Code:

(require 'org)
(require 'org-capture)

(defun pjs-gtd--set-agenda-file (sym val)
  (let* ((new-file (file-truename (concat org-directory "/" val)))
         (old-file (file-truename (concat org-directory "/" (or (ignore-errors (symbol-value sym))
                                                                (custom--standard-value sym))))))
    (set-default-toplevel-value sym val)
    (org-store-new-agenda-file-list (cons new-file (remove old-file org-agenda-files)))))

(defgroup pjs-gtd nil
  "Custom variables for pjs-gtd."
  :group 'org)

(defcustom pjs-gtd-inbox-file-name "in.org"
  "File containing inbox items.
This is where `org-capture' items should go.  Inbox is processed using
`pjs-gtd-process-inbox'.

File is relative to `org-directory'.  Added to `org-agenda-files' when set."
  :group 'pjs-gtd
  :type 'string)

(defcustom pjs-gtd-todo-file-name "todo.org"
  "File containing projects and tasks.
File is relative to `org-directory'.  Added to `org-agenda-files' when set."
  :group 'pjs-gtd
  :type 'string
  :set #'pjs-gtd--set-agenda-file)

(defcustom pjs-gtd-habits-file-name "habits.org"
  "File containing habits.
File is relative to `org-directory'.  Added to `org-agenda-files' when set."
  :group 'pjs-gtd
  :type 'string
  :set #'pjs-gtd--set-agenda-file)

(defcustom pjs-gtd-someday-file-name "someday.org"
  "File containing someday tasks.
Someday tasks are actionable tasks to do in the future.

File is relative to `org-directory'.  Added to `org-agenda-files' when set."
  :group 'pjs-gtd
  :type 'string
  :set #'pjs-gtd--set-agenda-file)

(defcustom pjs-gtd-maybe-file-name "maybe.org"
  "File containing maybe tasks.
Maybe tasks are actionable tasks that may not be done.

File is relative to `org-directory'.  Added to `org-agenda-files' when set."
  :group 'pjs-gtd
  :type 'string
  :set #'pjs-gtd--set-agenda-file)

(defcustom pjs-gtd-reference-file-name "reference.org"
  "File containing reference items.
Reference items are non-actionable items to keep for reference.

File is relative to `org-directory'."
  :group 'pjs-gtd
  :type 'string)

(defcustom pjs-gtd-archive-file-name "archive.org"
  "File containing archived tasks.
File is relative to `org-directory'.  Added to `org-agenda-files' when set."
  :group 'pjs-gtd
  :type 'string
  :set #'pjs-gtd--set-agenda-file)

(defun pjs-gtd-org-file (name)
  (concat org-directory "/" name))

(defun pjs-gtd-org-buffer (file)
  (let ((file (pjs-gtd-org-file file)))
    (or (find-buffer-visiting file)
        (find-file-noselect file))))

(defvar pjs-gtd--inactive-todo-states
  '("WAIT" "CANC" "DONE"))

(defvar pjs-gtd--inactive-tags
  '("WAIT" "HABIT" "SOMEDAY" "MAYBE" "CANC" "DONE" "ARCHIVE" "REFERENCE"))

(defun pjs-gtd-active-p ()
  (and (not (seq-intersection pjs-gtd--inactive-todo-states (list (org-get-todo-state))))
       (not (seq-intersection pjs-gtd--inactive-tags (org-get-tags)))))

(defun pjs-gtd-action-p ()
  (and (pjs-gtd-active-p)
       (not (equal "habit" (org-entry-get nil "STYLE")))
       (not (org-get-deadline-time nil))
       (not (org-get-scheduled-time nil))))

(defvar-local pjs-gtd--task-regexp nil)

(defun pjs-gtd-set-regexp ()
  (setq-local pjs-gtd--task-regexp (concat "^\\(\\*+\\)\\(?: +" org-todo-regexp "\\)")))

(defun pjs-gtd-project-p ()
  ;; top-level non-todo (top-level TODOs are singleton tasks)
  (or (and (= 1 (org-outline-level)) (not (org-get-todo-state)))
      ;; any-level TODO with a task child
      (let ((eos (save-excursion (org-end-of-subtree) (point))))
        (save-excursion
          (outline-end-of-heading)
          (search-forward-regexp pjs-gtd--task-regexp eos t)))))

(defvar org-state)
(defvar org-last-state)

(defun pjs-gtd-add-delegatee-to-waiting ()
  (if (equal org-state "WAIT")
      (let ((delegatee (org-read-property-value "DELEGATEE")))
        (org-set-property "DELEGATEE" delegatee)
        (org-add-log-setup 'state org-state org-last-state nil (concat "Delegated to " delegatee)))
    (when (org-entry-get nil "DELEGATEE")
      (org-delete-property "DELEGATEE"))))

(defun pjs-gtd-add-created-to-todo ()
  (when org-state
    (when (not (org-entry-get nil "CREATED"))
      (org-set-property "CREATED"
                        (format-time-string (org-time-stamp-format t t) (current-time))))))

(defun pjs-gtd-capture ()
  "Quickly capture a TODO item."
  (interactive)
  (let ((org-capture-templates
         '(("z" "Todo" entry
            (file "")
            "* TODO %?\12:PROPERTIES:\12:CREATED: %U\12:END:"))))
    (org-capture nil "z")))

(deftheme pjs-gtd)
(custom-theme-set-variables
 'pjs-gtd
 '(org-after-todo-state-change-hook '(pjs-gtd-add-delegatee-to-waiting pjs-gtd-add-created-to-todo))
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
(provide-theme 'pjs-gtd)
(enable-theme 'pjs-gtd)

(provide 'pjs-gtd)
;;; pjs-gtd.el ends here
