;;; tfl-ol-obsidian.el --- Org-link configuration for obsidian links     -*- lexical-binding: t; -*-
;;
;; Copyright (c) 2024 Paul Stadig
;;
;; Author: Paul Stadig <paul@thoughtfull.systems>
;;
;;; Commentary:
;;
;; Org-link configuration for obsidian links.
;;
;;; Code:

(use-package ol
  :after org
  :commands (org-link-set-parameters)
  :config
  (org-link-set-parameters "obsidian" :follow #'tfl-open-obsidian-link))

;;;###autoload
(defun tfl-open-obsidian-link (url _)
  "Open an \"obsidian\" type link.
URL is the obsidian url to open."
  (start-process "obsidian" nil "obsidian" (concat "obsidian:" url)))

(provide 'tfl-ol-obsidian)
;;; tfl-ol-obsidian.el ends here
