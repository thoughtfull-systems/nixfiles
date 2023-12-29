;;; pjs-org-bullets.el --- Use fancy bullets for headings and lists  -*- lexical-binding: t; -*-

;;  Copyright (c) 2023 Paul Stadig

;;  Version: 0.0.0

;;; Commentary:

;; Use fancy bullets for headings and lists.

;;; Code:
;; Heading bullets
(defun pob--compose-bullet (bullet)
  (prog1 () (compose-region (match-beginning 1) (match-end 1) bullet)))

(font-lock-add-keywords
 'org-mode
 '(("^\\(\\*\\) " 1 (pob--compose-bullet "●"))
   ("^\\*\\{1\\}\\(\\*\\) " 1 (pob--compose-bullet "◉"))
   ("^\\*\\{2\\}\\(\\*\\) " 1 (pob--compose-bullet "○"))
   ("^\\*\\{3\\}\\(\\*\\) " 1 (pob--compose-bullet "◆"))
   ("^\\*\\{4\\}\\(\\*\\) " 1 (pob--compose-bullet "◇"))
   ("^\\*\\{5\\}\\(\\*\\) " 1 (pob--compose-bullet "►"))
   ("^\\*\\{6\\}\\(\\*\\) " 1 (pob--compose-bullet "●"))
   ("^\\*\\{7\\}\\(\\*\\) " 1 (pob--compose-bullet "◉"))))

;; Plain list bullets
(font-lock-add-keywords
 'org-mode
 '(("^ *\\(?1:[-]\\) " 1 (pob--compose-bullet "•"))
   ("^ *\\(?1:[+]\\) " 1 (pob--compose-bullet "‣"))
   ("^ +\\(?1:[*]\\) " 1 (pob--compose-bullet "⁃"))))

(deftheme pjs-org-bullets)
(custom-theme-set-variables
 'pjs-org-bullets
 '(org-hide-leading-stars t)
 '(org-list-allow-alphabetical t)
 '(org-list-demote-modify-bullet '(("-" . "+")
                                   ("+" . "*")
                                   ("*" . "-")
                                   ("A." . "1.")
                                   ("1." . "a)")
                                   ("a)" . "1)")
                                   ("1)" . "A.")))
 '(org-startup-indented t))
(provide-theme 'pjs-org-bullets)
(enable-theme 'pjs-org-bullets)

(provide 'pjs-org-bullets)
;;; pjs-org-bullets.el ends here

;; Local Variables:
;; read-symbol-shorthands: (("pob-" . "pjs-org-bullets-"))
;; End:
