;;; tfl-gtd-agenda.el --- Organize GTD projects                          -*- lexical-binding: t; -*-

;;  Copyright (c) 2023 Paul Stadig

;;  Version: 0.0.0

;;; Commentary:

;; Take each unorganized project in a narrowed view for planning.

;;; Code:
(require 'tfl-gtd)
(require 'org-agenda)

(defun pga--previous-heading-same-level ()
  (org-back-to-heading)
  (let ((curr (point)))
    (org-backward-heading-same-level 1)
    (not (eq curr (point)))))

(defun pga--next-heading-same-level ()
  (org-back-to-heading)
  (let ((curr (point)))
    (org-forward-heading-same-level 1)
    (not (eq curr (point)))))

(defun pga--preceeding-next-action-p ()
  (save-excursion
    (let ((continue (pga--previous-heading-same-level))
          (result nil))
      (while (and (not result) continue)
        (when (and (not (tfl-gtd-project-p)) (org-get-todo-state) (tfl-gtd-action-p))
          (setq result t))
        (setq continue (pga--previous-heading-same-level)))
      result)))

(defun pga--next-action-p ()
  (and (not (tfl-gtd-project-p))
       (org-get-todo-state)
       (tfl-gtd-action-p)))

(defun pga-only-next-action ()
  (when (or (not (pga--next-action-p))
            (and (not (eq 1 (org-current-level)))
                 (pga--preceeding-next-action-p)))
    (let ((continue (outline-next-heading)))
      (while continue
        (if (or (not (pga--next-action-p))
                (and (not (eq 1 (org-current-level)))
                     (pga--preceeding-next-action-p)))
            (setq continue (outline-next-heading))
          (setq continue nil)
          (org-back-to-heading)))
      (point))))

(defun pga-daily ()
  "Show Agenda with next actions."
  (interactive)
  (let* ((org-agenda-custom-commands
          '(("z" "Agenda and all TODOs"
             ((agenda "")
              (alltodo ""
                       ((org-agenda-skip-function 'pga-only-next-action))))
             ((org-agenda-prefix-format
               '((agenda . " %i %?-12t% s%b")
                 (todo . " %i %b")
                 (tags . " %i %b")
                 (search . " %i %b")))
              (org-agenda-tag-filter-preset
               '("-ARCHIVE" "-DONE")))))))
    (org-agenda nil "z")))

;; (deftheme tfl-gtd-agenda)
;; (custom-theme-set-variables
;;  'tfl-gtd-agenda
;;  )
;; (provide-theme 'tfl-gtd-agenda)
;; (enable-theme 'tfl-gtd-agenda)

(provide 'tfl-gtd-agenda)
;;; tfl-gtd-agenda.el ends here

;; Local Variables:
;; read-symbol-shorthands: (("pga-" . "tfl-gtd-agenda-"))
;; End:
