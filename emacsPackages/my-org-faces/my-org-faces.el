;;; my-org-faces.el --- Faces for org-mode  -*- lexical-binding: t; -*-

;;  Copyright (c) 2023 Paul Stadig

;;  Version: 0.0.0

;;; Commentary:

;; Use variable pitch face for org-mode, making prefixes fixed width so things align properly.
;; Also, define strike through and highlight faces.

;;; Code:

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
 '(("^\\(\\* \\)"
    (1 'org-level-1-bullet))))

(font-lock-add-keywords
 'org-mode
 '(("^\\*\\{1\\}\\(\\* \\)"
    (1 'org-level-2-bullet))))

(font-lock-add-keywords
 'org-mode
 '(("^\\*\\{2\\}\\(\\* \\)"
    (1 'org-level-3-bullet))))

(font-lock-add-keywords
 'org-mode
 '(("^\\*\\{3\\}\\(\\* \\)"
    (1 'org-level-4-bullet))))

(font-lock-add-keywords
 'org-mode
 '(("^\\*\\{4\\}\\(\\* \\)"
    (1 'org-level-5-bullet))))

(font-lock-add-keywords
 'org-mode
 '(("^\\*\\{5\\}\\(\\* \\)"
    (1 'org-level-6-bullet))))

(font-lock-add-keywords
 'org-mode
 '(("^\\*\\{6\\}\\(\\* \\)"
    (1 'org-level-7-bullet))))

(font-lock-add-keywords
 'org-mode
 '(("^\\*\\{7\\}\\(\\* \\)"
    (1 'org-level-8-bullet))))

;;; Plain list bullets
;; Use a fancy bullet for '*', and make the whole prefix fixed-pitch so plain lists indent
;; consistently
(font-lock-add-keywords
 'org-mode
 '(("^ *- "
    (0 'fixed-pitch))))

(font-lock-add-keywords
 'org-mode
 '(("^ *+ "
    (0 'fixed-pitch))))

(font-lock-add-keywords
 'org-mode
 '(("^ *[0-9][.)] "
    (0 'fixed-pitch))))

(font-lock-add-keywords
 'org-mode
 '(("^ *[a-zA-Z][.)] "
    (0 'fixed-pitch))))

(font-lock-add-keywords
 'org-mode
 '(("^ +[*] "
    (0 'fixed-pitch))))

;;; Checkbox
;; Make the space after a checkbox fixed-pitch, so plain lists with checkboxes indent
;; consistently.
(font-lock-add-keywords
 'org-mode
 '(("[-+*.)] \\[[ X-]\\]\\( \\)"
    (1 'fixed-pitch))))

(defface org-strike-through '((t :foreground "gray" :strike-through t))
  "Face used for strike through text."
  :group 'org-faces)

(defface org-highlight '((t :background "yellow"))
  "Face used for highlighted text."
  :group 'org-faces)

;; This is font-lock, because the emphasis parsing in org-mode is more
;; complicated than just customizing org-emphasis-alist. The parsing is
;; integrated pretty deeply.
(font-lock-add-keywords
 'org-mode
 '(("\\(\\^[^\\^|]+\\^\\)"
    (1 'org-highlight))))

(deftheme my-org-faces)
(custom-theme-set-faces
 'my-org-faces
 '(org-block ((t (:inherit fixed-pitch))))
 '(org-checkbox ((t (:inherit fixed-pitch :weight bold :height 0.9))))
 '(org-code ((t (:inherit (shadow fixed-pitch)))))
 '(org-date ((t (:inherit fixed-pitch :foreground "Purple" :underline t))))
 '(org-document-info-keyword ((t (:inherit (shadow fixed-pitch)))))
 '(org-drawer ((t (:inherit fixed-pitch :foreground "Blue1"))))
 '(org-hide ((t (:inherit fixed-pitch :foreground "white"))))
 '(org-indent ((t (:inherit (org-hide fixed-pitch)))))
 '(org-link ((t (:inherit link :underline t))))
 '(org-meta-line ((t (:inherit (font-lock-comment-face fixed-pitch)))))
 '(org-property-value ((t (:inherit fixed-pitch))) t)
 '(org-special-keyword ((t (:inherit (font-lock-keyword-face fixed-pitch)))))
 '(org-table ((t (:inherit fixed-pitch :foreground "Blue1"))))
 '(org-tag ((t (:inherit fixed-pitch :weight bold))))
 '(org-verbatim ((t (:inherit (shadow fixed-pitch))))))
(provide-theme 'my-org-faces)
(enable-theme 'my-org-faces)

(provide 'my-org-faces)
;;; my-org-faces.el ends here
