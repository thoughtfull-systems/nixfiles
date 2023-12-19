;;; my-org.el --- Code and configuration for org-mode  -*- lexical-binding: t; -*-

;;  Copyright (c) 2023 Paul Stadig

;;  Version: 0.0.0

;;; Commentary:

;; Code and configuration for org-mode.

;;; Code:

(use-package org
  :hook ((org-mode . variable-pitch-mode))
  :bind (("C-c o b" . org-switchb)
         ("C-c o o" . org-cycle-agenda-files)
         ("C-c o j r". org-refile-goto-last-stored)
         ("C-c o j c". org-capture-goto-last-stored)))
(use-package org-capture
  :bind (("C-c o c" . org-capture)))
(use-package org-habit
  :after (org))
(use-package org-id
  :after (org))
(use-package org-protocol
  :after (org))

(deftheme my-org)
(custom-theme-set-variables
 'my-org
 '(org-archive-file-header-format "")
 '(org-archive-location "archive.org::")
 '(org-attach-id-dir "attachments/")
 '(org-directory "~/org")
 '(org-enforce-todo-checkbox-dependencies t)
 '(org-enforce-todo-dependencies t)
 '(org-habit-following-days 1)
 '(org-habit-graph-column 97)
 '(org-habit-preceding-days 14)
 '(org-id-link-to-org-use-id t)
 '(org-startup-folded 'content)
 '(org-yank-adjusted-subtrees t))
(provide-theme 'my-org)
(enable-theme 'my-org)

(provide 'my-org)
;;; my-org.el ends here
