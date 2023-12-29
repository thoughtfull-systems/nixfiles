;;; pjs-org-capture.el --- Code and configuration for org-mode  -*- lexical-binding: t; -*-

;;  Copyright (c) 2023 Paul Stadig

;;  Version: 0.0.0

;;; Commentary:

;; Code and configuration for org-mode.

;;; Code:

(use-package org-capture
  :bind (("C-c o c" . org-capture)))
(use-package org-protocol
  :after (org))

(deftheme pjs-org-capture)
(custom-theme-set-variables
 'pjs-org-capture
 '(org-capture-templates
   '(("t" "Todo" entry
      (file "")
      "* TODO %?
:PROPERTIES:
:CREATED: %U
:END:")
     ("l" "Linked Todo" entry
      (file "")
      "* TODO %?
:PROPERTIES:
:CREATED: %U
:END:
#+BEGIN_QUOTE
%i
#+END_QUOTE

From: %a")))
 '(org-default-notes-file "~/org/in.org"))
(provide-theme 'pjs-org-capture)
(enable-theme 'pjs-org-capture)

(provide 'pjs-org-capture)
;;; pjs-org-capture.el ends here
