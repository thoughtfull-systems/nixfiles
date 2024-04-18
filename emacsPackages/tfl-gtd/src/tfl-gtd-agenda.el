;;; tfl-gtd-agenda.el --- Organize GTD projects                          -*- lexical-binding: t; -*-

;;  Copyright (c) 2023 Technoophist

;;  Version: 0.0.0

;;; Commentary:

;; Take each unorganized project in a narrowed view for planning.

;;; Code:
(require 'tfl-gtd)
(require 'tfl-gtd-core)
(require 'org-agenda)

(defvar tga--buffer-name "*GTD Daily Agenda*")
(defvar tga--buffer nil)

(defun tga--previous-heading-same-level ()
  (org-back-to-heading)
  (let ((curr (point)))
    (org-backward-heading-same-level 1)
    (not (eq curr (point)))))

(defun tga--next-heading-same-level ()
  (org-back-to-heading)
  (let ((curr (point)))
    (org-forward-heading-same-level 1)
    (not (eq curr (point)))))

(defun tga--preceeding-next-action-p ()
  (save-excursion
    (let ((continue (tga--previous-heading-same-level))
          (result nil))
      (while (and (not result) continue)
        (when (and (not (tfl-gtd-project-p)) (org-get-todo-state) (tfl-gtd-action-p))
          (setq result t))
        (setq continue (tga--previous-heading-same-level)))
      result)))

(defun tga--next-action-p ()
  (and (not (tfl-gtd-project-p))
       (org-get-todo-state)
       (tfl-gtd-action-p)))

(defun tga-only-next-action ()
  (when (or (not (tga--next-action-p))
            (and (not (eq 1 (org-current-level)))
                 (tga--preceeding-next-action-p)))
    (let ((continue (outline-next-heading)))
      (while continue
        (if (or (not (tga--next-action-p))
                (and (not (eq 1 (org-current-level)))
                     (tga--preceeding-next-action-p)))
            (setq continue (outline-next-heading))
          (setq continue nil)
          (org-back-to-heading)))
      (point))))

(defun tga-daily ()
  "Show Agenda with next actions."
  (interactive)
  (if (buffer-live-p tga--buffer)
      (switch-to-buffer tga--buffer)
    (let* ((org-agenda-custom-commands
            `(("z" "Agenda and all TODOs"
               ((agenda "")
                (alltodo ""
                         ((org-agenda-skip-function 'tga-only-next-action))))
               ((org-agenda-buffer-name
                 ,tga--buffer-name)
                (org-agenda-prefix-format
                 '((agenda . " %i %?-12t% s%b")
                   (todo . " %i %b")
                   (tags . " %i %b")
                   (search . " %i %b")))
                (org-agenda-tag-filter-preset
                 '("-ARCHIVE" "-DONE" "-CANC"))
                (org-agenda-window-setup 'current-window))))))
      (org-agenda nil "z")
      (setq tga--buffer (current-buffer)))))

;; (deftheme tfl-gtd-agenda)
;; (custom-theme-set-variables
;;  'tfl-gtd-agenda
;;  )
;; (provide-theme 'tfl-gtd-agenda)
;; (enable-theme 'tfl-gtd-agenda)

(provide 'tfl-gtd-agenda)
;;; tfl-gtd-agenda.el ends here

;; Local Variables:
;; read-symbol-shorthands: (("tga-" . "tfl-gtd-agenda-"))
;; End:
