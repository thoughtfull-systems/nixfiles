;;; my-completion.el --- minibuffer completion  -*- lexical-binding: t; -*-

;; Copyright (c) 2023 Paul Stadig

;; Version: 0.0.0

;;; Commentary:

;; None

;;; Code:

(defun my-completion-delete-back-to-slash ()
  "Delete characters backward until a slash."
  (interactive)
  (let ((end (point-marker)))
    (when (string= (char-to-string (char-before)) "/")
      (backward-char 1))
    (if (search-backward "/" nil t)
        (progn
          (forward-char 1)
          (delete-region (point-marker) end))
      (goto-char end))))

(use-package icomplete
  :bind
  (:map icomplete-minibuffer-map
        ("C-<return>" . icomplete-force-complete)
        ("M-<return>" . exit-minibuffer)
        ("<return>" . icomplete-force-complete-and-exit)
        ("C-l" . my-completion-delete-back-to-slash)))
(use-package marginalia
  :hook (icomplete-mode . marginalia-mode))
(use-package orderless)

(deftheme my-completion)
(custom-theme-set-variables
 'my-completion
 '(completion-auto-help nil)
 '(completion-category-overrides '((file (styles basic partial-completion orderless))))
 '(completion-cycle-threshold 3)
 '(completion-styles '(orderless))
 '(completions-detailed t)
 '(icomplete-show-matches-on-no-input t)
 '(icomplete-vertical-mode t)
 '(orderless-matching-styles
   '(orderless-regexp orderless-literal orderless-initialism orderless-prefixes))
 '(read-buffer-completion-ignore-case t)
 '(read-file-name-completion-ignore-case t))
(custom-theme-set-faces
 'my-completion
 '(completions-common-part ((t (:inherit orderless-match-face-0))))
 '(icomplete-selected-match ((t nil))))
(provide-theme 'my-completion)
(enable-theme 'my-completion)

(provide 'my-completion)
;;; my-completion.el ends here
