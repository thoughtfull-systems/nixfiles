;;; my-completion.el --- Incremental minibuffer completion                -*- lexical-binding: t -*-
;;
;; Copyright (c) 2023 Paul Stadig
;;
;; Author: Paul Stadig <paul@thoughtfull.systems>
;;

;;; Commentary:
;;
;; Configure incremental minibuffer completion pretty much everywhere in Emacs.
;;

;;; Code:

;;; Dependencies
(require 'my-core)
(use-package all-the-icons-completion
  :after (all-the-icons icomplete marginalia)
  :commands (all-the-icons-completion-mode)
  :config (all-the-icons-completion-mode)
  :hook (marginalia-mode . all-the-icons-completion-marginalia-setup))
(use-package icomplete
  :bind
  (:map icomplete-minibuffer-map
        ("C-<return>" . icomplete-force-complete)
        ("M-<return>" . #'exit-minibuffer)
        ("<return>" . icomplete-force-complete-and-exit)
        ("C-l" . #'my-completion-delete-back-to-slash)))
(use-package marginalia
  :hook (icomplete-mode . marginalia-mode))


;;; Functions
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


;;; Configuration
(my-custom-theme-set-variables
 'my
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
(my-custom-theme-set-faces
 'my
 '(completions-common-part ((t (:inherit orderless-match-face-0))))
 '(icomplete-selected-match ((t nil))))

(provide 'my-completion)
;;; my-completion.el ends here
