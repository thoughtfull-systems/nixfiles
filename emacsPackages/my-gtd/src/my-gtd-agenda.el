;;; my-gtd-agenda.el --- Organize GTD projects  -*- lexical-binding: t; -*-

;;  Copyright (c) 2023 Paul Stadig

;;  Version: 0.0.0

;;; Commentary:

;; Take each unorganized project in a narrowed view for planning.

;;; Code:
(require 'my-gtd)
(require 'org-agenda)

(defun mga--previous-heading-same-level ()
  (org-back-to-heading)
  (let ((curr (point)))
    (org-backward-heading-same-level 1)
    (not (eq curr (point)))))

(defun mga--next-heading-same-level ()
  (org-back-to-heading)
  (let ((curr (point)))
    (org-forward-heading-same-level 1)
    (not (eq curr (point)))))

(defun mga--preceeding-next-action-p ()
  (save-excursion
    (let ((continue (mga--previous-heading-same-level))
          (result nil))
      (while (and (not result) continue)
        (when (and (not (my-gtd-project-p)) (org-get-todo-state) (my-gtd-action-p))
          (setq result t))
        (setq continue (mga--previous-heading-same-level)))
      result)))

(defun mga--next-action-p ()
  (and (not (my-gtd-project-p))
       (org-get-todo-state)
       (my-gtd-action-p)))

(defun mga-only-next-action ()
  (when (or (not (mga--next-action-p))
            (and (not (eq 1 (org-current-level)))
                 (mga--preceeding-next-action-p)))
    (let ((continue (outline-next-heading)))
      (while continue
        (if (or (not (mga--next-action-p))
                (and (not (eq 1 (org-current-level)))
                     (mga--preceeding-next-action-p)))
            (setq continue (outline-next-heading))
          (setq continue nil)
          (org-back-to-heading)))
      (point))))

(defun mga-daily ()
  "Show Agenda with next actions."
  (interactive)
  (let* ((org-agenda-custom-commands
          '(("z" "Agenda and all TODOs"
             ((agenda ""
                      ((org-agenda-tag-filter-preset
                        '("-ARCHIVE"))))
              (alltodo ""
                       ((org-agenda-skip-function 'mga-only-next-action))))
             ((org-agenda-prefix-format
               '((agenda . " %i %?-12t% s%b")
                 (todo . " %i %b")
                 (tags . " %i %b")
                 (search . " %i %b"))))))))
    (org-agenda nil "z")))

;; (deftheme my-gtd-agenda)
;; (custom-theme-set-variables
;;  'my-gtd-agenda
;;  )
;; (provide-theme 'my-gtd-agenda)
;; (enable-theme 'my-gtd-agenda)

(provide 'my-gtd-agenda)
;;; my-gtd-agenda.el ends here

;; Local Variables:
;; read-symbol-shorthands: (("mga-" . "my-gtd-agenda-"))
;; End:
