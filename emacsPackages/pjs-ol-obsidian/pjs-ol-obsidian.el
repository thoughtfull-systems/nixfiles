;;; pjs-ol-obsidian.el --- Org-link configuration for obsidian links  -*- lexical-binding: t; -*-

;;  Copyright (c) 2023 Paul Stadig

;;  Version: 0.0.0

;;; Commentary:

;; Org-link configuration for obsidian links.

;;; Code:

(use-package ol
  :after org
  :commands (org-link-set-parameters)
  :config
  (org-link-set-parameters "obsidian" :follow #'pjs-open-obsidian-link))

;;;###autoload
(defun pjs-open-obsidian-link (url _)
  "Open an \"obsidian\" type link.
URL is the obsidian url to open."
  (shell-command (concat "obsidian \"obsidian:" url "\"") nil nil))

(provide 'pjs-ol-obsidian)
;;; pjs-ol-obsidian.el ends here
