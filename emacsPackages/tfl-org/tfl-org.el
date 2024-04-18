;;; tfl-org.el --- Code and configuration for org-mode                   -*- lexical-binding: t; -*-
;;
;;  Copyright (c) 2024 Technosophist
;;
;;; Commentary:
;;
;; Code and configuration for org-mode.
;;
;;; Code:

(use-package hl-line
  :hook (org-agenda-mode . hl-line-mode))
(use-package org
  :bind (("C-c o b" . org-switchb)
         ("C-c o o" . org-cycle-agenda-files)
         ("C-c o j r". org-refile-goto-last-stored)
         ("C-c o j c". org-capture-goto-last-stored))
  :hook ((org-mode . (lambda () (buffer-face-toggle 'variable-pitch-text)))))
(use-package org-autolist
  :after (org)
  :diminish
  :hook (org-mode . org-autolist-mode))
(use-package org-habit
  :after (org))
(use-package org-id
  :after (org))
(use-package org-indent
  :after (org)
  :diminish)
(use-package org-protocol
  :after (org))

(deftheme tfl-org)
(custom-theme-set-variables
 'tfl-org
 '(org-agenda-skip-deadline-if-done t)
 '(org-agenda-skip-deadline-prewarning-if-scheduled 'pre-scheduled)
 '(org-agenda-skip-scheduled-if-deadline-is-shown 'repeated-after-deadline)
 '(org-agenda-skip-scheduled-if-done t)
 '(org-agenda-skip-timestamp-if-deadline-is-shown t)
 '(org-agenda-skip-timestamp-if-done t)
 '(org-archive-file-header-format "")
 '(org-archive-location "archive.org::")
 '(org-attach-id-dir "attachments/")
 '(org-autolist-enable-delete t)
 '(org-directory "~/org")
 ;; 'm' is 'month' by default, which seems crazy to me.  I'm usually estimating fine-grained tasks,
 ;; so I'm not sure I even need 'w', but leaving it here for now.
 '(org-duration-units '(("m" . 1) ("h" . 60) ("d" . 480) ("w" . 2400)))
 '(org-enforce-todo-checkbox-dependencies t)
 '(org-enforce-todo-dependencies t)
 '(org-habit-following-days 0)
 '(org-habit-graph-column 92)
 '(org-habit-preceding-days 14)
 '(org-id-link-to-org-use-id t)
 '(org-startup-folded 'content)
 '(org-tag-alist '(("MAYBE" . ?m)
                   (:startgroup)
                   ("SOMEDAY" . ?s)
                   (:grouptags)
                   ("TOLISTEN" . ?l)
                   ("TOREAD" . ?r)
                   ("TOWATCH" . ?w)
                   (:endgroup)
                   ("REFERENCE" . ?e)))
 '(org-treat-insert-todo-heading-as-state-change t)
 '(org-yank-adjusted-subtrees t))
(provide-theme 'tfl-org)
(enable-theme 'tfl-org)

(provide 'tfl-org)
;;; tfl-org.el ends here
