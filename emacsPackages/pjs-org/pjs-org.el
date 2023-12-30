;;; pjs-org.el --- Code and configuration for org-mode                   -*- lexical-binding: t; -*-

;;  Copyright (c) 2023 Paul Stadig

;;  Version: 0.0.0

;;; Commentary:

;; Code and configuration for org-mode.

;;; Code:

(use-package org
  :bind (("C-c o b" . org-switchb)
         ("C-c o o" . org-cycle-agenda-files)
         ("C-c o j r". org-refile-goto-last-stored)
         ("C-c o j c". org-capture-goto-last-stored))
  :hook ((org-mode . variable-pitch-mode)))
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

(deftheme pjs-org)
(custom-theme-set-variables
 'pjs-org
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
 '(org-habit-following-days 1)
 '(org-habit-graph-column 97)
 '(org-habit-preceding-days 14)
 '(org-id-link-to-org-use-id t)
 '(org-startup-folded 'content)
 '(org-yank-adjusted-subtrees t))
(provide-theme 'pjs-org)
(enable-theme 'pjs-org)

(provide 'pjs-org)
;;; pjs-org.el ends here
