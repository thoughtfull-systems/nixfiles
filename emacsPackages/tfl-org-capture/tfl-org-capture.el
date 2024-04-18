;;; tfl-org-capture.el --- Code and configuration for org-capture        -*- lexical-binding: t; -*-
;;
;;  Copyright (c) 2024 Technosophist
;;
;;  Version: 0.0.0
;;
;;; Commentary:
;;
;; Code and configuration for org-capture.
;;
;;; Code:

(use-package org-capture
  :bind (("C-c o c" . org-capture)))

(deftheme tfl-org-capture)
(custom-theme-set-variables
 'tfl-org-capture
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
(provide-theme 'tfl-org-capture)
(enable-theme 'tfl-org-capture)

(provide 'tfl-org-capture)
;;; tfl-org-capture.el ends here
