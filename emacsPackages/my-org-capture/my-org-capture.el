;;; my-org-capture.el --- Code and configuration for org-mode  -*- lexical-binding: t; -*-

;;  Copyright (c) 2023 Paul Stadig

;;  Version: 0.0.0

;;; Commentary:

;; Code and configuration for org-mode.

;;; Code:

(use-package org-capture
  :bind (("C-c o c" . org-capture)))
(use-package org-protocol
  :after (org))

(deftheme my-org-capture)

(custom-theme-set-variables
 'my-org-capture
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

(provide-theme 'my-org-capture)
(enable-theme 'my-org-capture)

(provide 'my-org-capture)
;;; my-org-capture.el ends here
