;;; tfl-core.el --- Generally useful functions and such                  -*- lexical-binding: t; -*-
;;
;; Copyright (c) 2024 Technosophist
;;
;; Author: Technosophist <technosophist@thoughtfull.systems>
;;
;;; Commentary:
;;
;; A kind of a "miscellaneous" library for functions an such not having a home elsewhere, though the
;; ideally things functions and such move out to other libraries as seams become apparent.
;;
;;; Code:


;; General
(defun tfl-buffer-compare-name (b1 b2)
  "Compare buffer names of B1 and B2."
  (string-collate-lessp (buffer-name b1) (buffer-name b2) nil t))

(defun tfl-switch-to-most-recent-buffer ()
  "Switch to most recently used buffer."
  (interactive)
  (switch-to-buffer (other-buffer (current-buffer) t)))

(defun tfl-switch-buffer (&optional prefix)
  "Switch buffers using incremental completion.
When given PREFIX, use `ibuffer' instead."
  (interactive "p")
  (if (eq prefix 4)
      (ibuffer)
    (call-interactively #'switch-to-buffer)))

(defun tfl-kill-buffer-and-delete-file ()
  "Kill the current buffer and delete the file it is visiting."
  (interactive)
  (let ((filename (buffer-file-name)))
    (when filename
      (if (vc-backend filename)
          (vc-delete-file filename)
        (progn
          (delete-file filename)
          (message "Deleted file %s" filename)
          (kill-buffer))))))


;; Completion
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

(provide 'tfl-core)
;;; tfl-core.el ends here
