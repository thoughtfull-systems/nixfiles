;;; tfl.el --- Essentials for a pleasant Emacs experience                -*- lexical-binding: t; -*-
;;
;; Copyright (c) 2024 Paul Stadig
;;
;; Author: Paul Stadig <paul@thoughtfull.systems>
;;
;;; Commentary:
;;
;; Configures essential and/or global aspects of the Emacs experience.  Customizations broadly
;; affecting the Emacs experience should be here.  As well, this is kind of a "miscellaneous"
;; library for customizations not having a home elsewhere, though the ideally customizations should
;; move out to other libraries as seams become apparent.
;;
;;; Code:

(defun tfl-buffer-compare (b1 b2)
  "Compare buffer names of B1 and B2."
  (string-collate-lessp (buffer-name b1) (buffer-name b2) nil t))

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

(require 'tfl-completion)
(require 'tfl-prog)

(provide 'tfl)
;;; tfl.el ends here
