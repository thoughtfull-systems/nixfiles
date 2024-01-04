;;; tfl-completion.el --- Incremental completion everywhere              -*- lexical-binding: t; -*-
;;
;; Copyright (c) 2024 Paul Stadig
;;
;; Author: Paul Stadig <paul@thoughtfull.systems>
;;
;;; Commentary:
;;
;; Incremental completion everywhere using icomplete, marginalia, orderless and consult.
;;
;;; Code:


;;; Dependencies
(require 'tfl-core)
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
        ("C-l" . #'tfl-completion-delete-back-to-slash)))
(use-package marginalia
  :hook (icomplete-mode . marginalia-mode))


;;; Functions
(defun tfl-completion-delete-back-to-slash ()
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
(tfl-custom-theme-set-variables
 'tfl
 '(completion-auto-help nil)
 '(completion-category-overrides '((file (styles basic partial-completion orderless))))
 '(completion-cycle-threshold 3)
 '(completion-styles '(orderless))
 '(completions-detailed t)
 '(consult-after-jump-hook '(recenter pulse-momentary-highlight-one-line))
 '(consult-narrow-key "C-,")
 '(consult-preview-key nil)
 '(icomplete-show-matches-on-no-input t)
 '(icomplete-vertical-mode t)
 '(orderless-matching-styles
   '(orderless-regexp orderless-literal orderless-initialism orderless-prefixes))
 '(read-buffer-completion-ignore-case t)
 '(read-file-name-completion-ignore-case t))
(tfl-custom-theme-set-faces
 'tfl
 '(completions-common-part ((t (:inherit orderless-match-face-0))))
 '(icomplete-selected-match ((t nil))))

(provide 'tfl-completion)
;;; tfl-completion.el ends here
