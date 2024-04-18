;;; tfl-org-faces.el --- Faces for org-mode                              -*- lexical-binding: t; -*-
;;
;;  Copyright (c) 2024 Technosophist
;;
;;  Version: 0.0.0
;;
;;; Commentary:
;;
;; Because I like to use variable pitch for org-mode, make the left margins fixed pitch, so things
;; line up when indented.
;;
;;; Code:
;; Make heading bullets fixed pitch
(defface org-level-1-bullet '((t :inherit (org-level-1 fixed-pitch)))
  "Face used for level 1 headline bullets."
  :group 'org-faces)

(defface org-level-2-bullet '((t :inherit (org-level-2 fixed-pitch)))
  "Face used for level 1 headline bullets."
  :group 'org-faces)

(defface org-level-3-bullet '((t :inherit (org-level-3 fixed-pitch)))
  "Face used for level 3 headline bullets."
  :group 'org-faces)

(defface org-level-4-bullet '((t :inherit (org-level-4 fixed-pitch)))
  "Face used for level 4 headline bullets."
  :group 'org-faces)

(defface org-level-5-bullet '((t :inherit (org-level-5 fixed-pitch)))
  "Face used for level 5 headline bullets."
  :group 'org-faces)

(defface org-level-6-bullet '((t :inherit (org-level-6 fixed-pitch)))
  "Face used for level 6 headline bullets."
  :group 'org-faces)

(defface org-level-7-bullet '((t :inherit (org-level-7 fixed-pitch)))
  "Face used for level 7 headline bullets."
  :group 'org-faces)

(defface org-level-8-bullet '((t :inherit (org-level-8 fixed-pitch)))
  "Face used for level 8 headline bullets."
  :group 'org-faces)

(font-lock-add-keywords
 'org-mode
 '(("^\\(\\* \\)" 1 'org-level-1-bullet)
   ("^\\*\\{1\\}\\(\\* \\)" 1 'org-level-2-bullet)
   ("^\\*\\{2\\}\\(\\* \\)" 1 'org-level-3-bullet)
   ("^\\*\\{3\\}\\(\\* \\)" 1 'org-level-4-bullet)
   ("^\\*\\{4\\}\\(\\* \\)" 1 'org-level-5-bullet)
   ("^\\*\\{5\\}\\(\\* \\)" 1 'org-level-6-bullet)
   ("^\\*\\{6\\}\\(\\* \\)" 1 'org-level-7-bullet)
   ("^\\*\\{7\\}\\(\\* \\)" 1 'org-level-8-bullet)))

;; Make prefix spaces and plain list markers fixed-pitch so plain lists indent consistently
(font-lock-add-keywords
 'org-mode
 '(("^ +[*] " 0 'fixed-pitch)
   ("^ *[-] " 0 'fixed-pitch)
   ("^ *[+] " 0 'fixed-pitch)
   ("^ *[0-9]+[.)] " 0 'fixed-pitch)
   ("^ *[a-zA-Z][.)] " 0 'fixed-pitch)
   ("^\\( +\\)[^ ]" 1 'fixed-pitch)))

;; Fix face for checkboxes on alphabetical lists.
(font-lock-add-keywords
 'org-mode
 '(("^ *[a-zA-z][.)] \\(\\[@[a-zA-Z]\\] \\)?\\(\\[[ X-]\\]\\)" 2 'org-checkbox)))

(defface org-strike-through '((t :foreground "gray" :strike-through t))
  "Face used for strike through text."
  :group 'org-faces)

(deftheme tfl-org-faces)
(custom-theme-set-variables
 'tfl-org-faces
 `(org-emphasis-alist
   ,(list 'quote
          (cons (list "+" 'org-strike-through)
                (assoc-delete-all "+" org-emphasis-alist)))))
(custom-theme-set-faces
 'tfl-org-faces
 '(org-block ((t (:inherit fixed-pitch))))
 '(org-checkbox ((t (:inherit fixed-pitch :weight bold :height 0.9))))
 '(org-code ((t (:inherit (shadow fixed-pitch)))))
 '(org-date ((t (:inherit fixed-pitch :foreground "Purple" :underline t))))
 '(org-document-info-keyword ((t (:inherit (shadow fixed-pitch)))))
 '(org-done ((t (:inherit fixed-pitch :foreground "ForestGreen" :weight bold))))
 '(org-drawer ((t (:inherit fixed-pitch :foreground "gray"))))
 '(org-hide ((t (:inherit fixed-pitch :foreground "white"))))
 '(org-indent ((t (:inherit (org-hide fixed-pitch)))))
 '(org-link ((t (:inherit link :underline t))))
 '(org-meta-line ((t (:inherit (font-lock-comment-face fixed-pitch)))))
 '(org-property-value ((t (:inherit fixed-pitch))) t)
 '(org-special-keyword ((t (:inherit (font-lock-keyword-face fixed-pitch)))))
 '(org-table ((t (:inherit fixed-pitch :foreground "Blue1"))))
 '(org-tag ((t (:inherit fixed-pitch :weight bold))))
 '(org-todo ((t (:inherit fixed-pitch :foreground "Red1" :weight bold))))
 '(org-verbatim ((t (:inherit (shadow fixed-pitch))))))
(provide-theme 'tfl-org-faces)
(enable-theme 'tfl-org-faces)

(provide 'tfl-org-faces)
;;; tfl-org-faces.el ends here
