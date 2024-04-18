;;; tfl-org-bullets.el --- Use fancy bullets for headings and lists      -*- lexical-binding: t; -*-
;;
;;  Copyright (c) 2024 Technosophist
;;
;;; Commentary:
;;
;; Use fancy bullets for headings and lists.
;;
;;; Code:

;; Heading bullets
(defun tob--compose-bullet (bullet)
  (prog1 () (compose-region (match-beginning 1) (match-end 1) bullet)))

(font-lock-add-keywords
 'org-mode
 '(("^\\(\\*\\) " 1 (tob--compose-bullet "●"))
   ("^\\*\\{1\\}\\(\\*\\) " 1 (tob--compose-bullet "◉"))
   ("^\\*\\{2\\}\\(\\*\\) " 1 (tob--compose-bullet "○"))
   ("^\\*\\{3\\}\\(\\*\\) " 1 (tob--compose-bullet "◆"))
   ("^\\*\\{4\\}\\(\\*\\) " 1 (tob--compose-bullet "◇"))
   ("^\\*\\{5\\}\\(\\*\\) " 1 (tob--compose-bullet "►"))
   ("^\\*\\{6\\}\\(\\*\\) " 1 (tob--compose-bullet "●"))
   ("^\\*\\{7\\}\\(\\*\\) " 1 (tob--compose-bullet "◉"))))

;; Plain list bullets
(font-lock-add-keywords
 'org-mode
 '(("^ *\\(?1:[-]\\) " 1 (tob--compose-bullet "•"))
   ("^ *\\(?1:[+]\\) " 1 (tob--compose-bullet "‣"))
   ("^ +\\(?1:[*]\\) " 1 (tob--compose-bullet "⁃"))))

(deftheme tfl-org-bullets)
(custom-theme-set-variables
 'tfl-org-bullets
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
(provide-theme 'tfl-org-bullets)
(enable-theme 'tfl-org-bullets)

(provide 'tfl-org-bullets)
;;; tfl-org-bullets.el ends here

;; Local Variables:
;; read-symbol-shorthands: (("tob-" . "tfl-org-bullets-"))
;; End:
