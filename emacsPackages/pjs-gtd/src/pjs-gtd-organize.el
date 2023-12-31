;;; pjs-gtd-organize.el --- Organize GTD projects                        -*- lexical-binding: t; -*-

;;  Copyright (c) 2023 Paul Stadig

;;  Version: 0.0.0

;;; Commentary:

;; Take each unorganized project in a narrowed view for planning.

;;; Code:
(require 'pjs-gtd)
(require 'org-refile)
(require 'transient)

(defvar-keymap pgo-map
  :doc "Keymap for command `pjs-gtd-organize-mode', a minor mode."
  "C-c C-c" #'pgo-next)

(define-minor-mode pgo-mode
  "Minor mode for pjs-gtd-organize."
  :lighter " MGP"
  :keymap pgo-map
  :group 'pjs-gtd-organize
  (if pgo-mode
      (setq-local
       header-line-format
       `("Tasks should be SMART, singular, brief, verb-based!  "
         ,(substitute-command-keys "\\<pjs-gtd-organize-map>Finish `\\[pjs-gtd-organize-next]'.")))
    (setq-local header-line-format nil)))

(defvar pgo-buffer-name "*pjs-gtd-organize*")

;;;###autoload
(defun pjs-gtd-organize ()
  "Process each entry from inbox in a narrowed buffer for processing."
  (interactive)
  (if (get-buffer pgo-buffer-name)
      (pop-to-buffer pgo-buffer-name)
    (with-current-buffer
        (make-indirect-buffer (pjs-gtd-org-buffer pjs-gtd-todo-file-name) pgo-buffer-name t)
      (pgo-mode)
      (goto-char (point-min))
      (outline-next-heading)
      (pgo--next "No unorganized projects."))))

(defun pgo--next-heading-same-level ()
  (org-back-to-heading)
  (let ((curr (point)))
    (org-forward-heading-same-level 1)
    (not (eq curr (point)))))

(defun mgp--find-unorganized-project ()
  (let (unorganized
        (heading (point))
        (continue t)
        (eos (save-excursion (org-end-of-subtree) (point))))
    (outline-next-heading)
    (while (and (< (point) eos) continue)
      (if (pjs-gtd-project-p)
          (if (pjs-gtd-active-p)
              (let ((point (save-excursion (mgp--find-unorganized-project))))
                (if (not point)
                    ;; organized project, keep looking
                    (setq continue (pgo--next-heading-same-level))
                  ;; unorganized project, exit early
                  (setq unorganized point)
                  (setq continue nil)))
            (unless (pgo--next-heading-same-level)
              (setq unorganized nil)
              (setq continue nil)))
        (if (or (not (org-get-todo-state))
                (not (pjs-gtd-active-p)))
            (unless (pgo--next-heading-same-level)
              (setq unorganized heading)
              (setq continue nil))
          (setq unorganized nil)
          (setq continue nil))))
    unorganized))

(defun pgo--first-unorganized-project ()
  (goto-char (point-min))
  (outline-next-heading)
  (let ((organized t)
        (continue t))
    (while (and organized continue)
      (if (save-excursion
            (outline-next-heading)
            (not (eq 2 (org-current-level))))
          (setq organized nil)
        (if (not (or (not (pjs-gtd-project-p))
                     (and (pjs-gtd-project-p)
                          (pjs-gtd-active-p)
                          (pgo--organized-project-p))))
            (setq organized nil)
          (setq continue (pgo--next-heading-same-level)))))
    organized))

(defun pgo--next (&optional msg)
  (if (not (pgo--next-unorganized-project))
      (progn
        (message (or msg "No more unorganized projects."))
        (kill-buffer pgo-buffer-name))
    (org-fold-heading nil t)
    (org-narrow-to-subtree)
    (pop-to-buffer pgo-buffer-name)))

(defun pgo-next ()
  "Move to next unorganized project for planning.
If there are no more unorganized projects, show MSG or a default."
  (interactive)
  (if (not (save-excursion (goto-char (point-min)) (pgo--organized-project-p)))
      (message "Project is not organized!")
    (widen)
    (goto-char (point-min))
    (pgo--next)))

;; (deftheme pjs-gtd-organize)
;; (custom-theme-set-variables
;;  'pjs-gtd-organize
;;  )
;; (provide-theme 'pjs-gtd-organize)
;; (enable-theme 'pjs-gtd-organize)

(provide 'pjs-gtd-organize)
;;; pjs-gtd-organize.el ends here

;; Local Variables:
;; read-symbol-shorthands: (("pgo-" . "pjs-gtd-organize-"))
;; End:
