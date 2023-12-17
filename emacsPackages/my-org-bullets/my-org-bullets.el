;;; my-org-bullets.el --- Use fancy bullets for headings and lists  -*- lexical-binding: t; -*-

;;  Copyright (c) 2023 Paul Stadig

;;  Version: 0.0.0

;;; Commentary:

;; Use fancy bullets for headings and lists.

;;; Code:

(font-lock-add-keywords
 'org-mode
 '(("^\\(\\*\\) "
    (1 (prog1 () (compose-region (match-beginning 1) (match-end 1) "●"))))))

(font-lock-add-keywords
 'org-mode
 '(("^\\*\\{1\\}\\(\\*\\) "
    (1 (prog1 () (compose-region (match-beginning 1) (match-end 1) "◉"))))))

(font-lock-add-keywords
 'org-mode
 '(("^\\*\\{2\\}\\(\\*\\) "
    (1 (prog1 () (compose-region (match-beginning 1) (match-end 1) "○"))))))

(font-lock-add-keywords
 'org-mode
 '(("^\\*\\{3\\}\\(\\*\\) "
    (1 (prog1 () (compose-region (match-beginning 1) (match-end 1) "◆"))))))

(font-lock-add-keywords
 'org-mode
 '(("^\\*\\{4\\}\\(\\*\\) "
    (1 (prog1 () (compose-region (match-beginning 1) (match-end 1) "◇"))))))

(font-lock-add-keywords
 'org-mode
 '(("^\\*\\{5\\}\\(\\*\\) "
    (1 (prog1 () (compose-region (match-beginning 1) (match-end 1) "►"))))))

(font-lock-add-keywords
 'org-mode
 '(("^\\*\\{6\\}\\(\\*\\) "
    (1 (prog1 () (compose-region (match-beginning 1) (match-end 1) "●"))))))

(font-lock-add-keywords
 'org-mode
 '(("^\\*\\{7\\}\\(\\*\\) "
    (1 (prog1 () (compose-region (match-beginning 1) (match-end 1) "◉"))))))

;;; Plain list bullets
(font-lock-add-keywords
 'org-mode
 '(("^ +\\(?1:[*]\\) "
    (1 (prog1 () (compose-region (match-beginning 1) (match-end 1) "•"))))))

(deftheme my-org-bullets)

(custom-theme-set-variables
 'my-org-bullets
 '(org-hide-leading-stars t)
 '(org-list-demote-modify-bullet '(("-" . "+") ("+" . "*") ("*" . "-")))
 '(org-startup-indented t))

(provide-theme 'my-org-bullets)
(enable-theme 'my-org-bullets)

(provide 'my-org-bullets)
;;; my-org-bullets.el ends here
