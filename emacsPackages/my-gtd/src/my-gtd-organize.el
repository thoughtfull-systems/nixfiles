;;; my-gtd-organize.el --- Organize GTD projects  -*- lexical-binding: t; -*-

;;  Copyright (c) 2023 Paul Stadig

;;  Version: 0.0.0

;;; Commentary:

;; Take each unorganized project in a narrowed view for planning.

;;; Code:
(require 'my-gtd)
(require 'org-refile)
(require 'transient)

(defvar-keymap mgo-map
  :doc "Keymap for command `my-gtd-organize-mode', a minor mode."
  "C-c C-c" #'mgo-next)

(define-minor-mode mgo-mode
  "Minor mode for my-gtd-organize."
  :lighter " MGP"
  :keymap mgo-map
  :group 'my-gtd-organize
  (if mgo-mode
      (setq-local
       header-line-format
       `("Tasks should be SMART, singular, brief, verb-based!  "
         ,(substitute-command-keys "\\<my-gtd-organize-map>Finish `\\[my-gtd-organize-next]'.")))
    (setq-local header-line-format nil)))

(defvar mgo-buffer-name "*my-gtd-organize*")

;;;###autoload
(defun my-gtd-organize ()
  "Process each entry from inbox in a narrowed buffer for processing."
  (interactive)
  (if (get-buffer mgo-buffer-name)
      (pop-to-buffer mgo-buffer-name)
    (with-current-buffer
        (make-indirect-buffer (my-gtd-org-buffer my-gtd-todo-file-name) mgo-buffer-name t)
      (mgo-mode)
      (goto-char (point-min))
      (outline-next-heading)
      (mgo--next "No unorganized projects."))))

(defun mgo--next-heading-same-level ()
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
      (if (my-gtd-project-p)
          (if (my-gtd-active-p)
              (let ((point (save-excursion (mgp--find-unorganized-project))))
                (if (not point)
                    ;; organized project, keep looking
                    (setq continue (mgo--next-heading-same-level))
                  ;; unorganized project, exit early
                  (setq unorganized point)
                  (setq continue nil)))
            (when (not (mgo--next-heading-same-level))
              (setq unorganized nil)
              (setq continue nil)))
        (if (or (not (org-get-todo-state))
                (not (my-gtd-active-p)))
            (when (not (mgo--next-heading-same-level))
              (setq unorganized heading)
              (setq continue nil))
          (setq unorganized nil)
          (setq continue nil))))
    unorganized))

(defun mgo--first-unorganized-project ()
  (goto-char (point-min))
  (outline-next-heading)
  (let ((organized t)
        (continue t))
    (while (and organized continue)
      (if (save-excursion
            (outline-next-heading)
            (not (eq 2 (org-current-level))))
          (setq organized nil)
        (if (not (or (not (my-gtd-project-p))
                     (and (my-gtd-project-p)
                          (my-gtd-active-p)
                          (mgo--organized-project-p))))
            (setq organized nil)
          (setq continue (mgo--next-heading-same-level)))))
    organized))

(defun mgo--next (&optional msg)
  (if (not (mgo--next-unorganized-project))
      (progn
        (message (or msg "No more unorganized projects."))
        (kill-buffer mgo-buffer-name))
    (org-fold-heading nil t)
    (org-narrow-to-subtree)
    (pop-to-buffer mgo-buffer-name)))

(defun mgo-next ()
  "Move to next unorganized project for planning.
If there are no more unorganized projects, show MSG or a default."
  (interactive)
  (if (not (save-excursion (goto-char (point-min)) (mgo--organized-project-p)))
      (message "Project is not organized!")
    (widen)
    (goto-char (point-min))
    (mgo--next)))

;; (deftheme my-gtd-organize)
;; (custom-theme-set-variables
;;  'my-gtd-organize
;;  )
;; (provide-theme 'my-gtd-organize)
;; (enable-theme 'my-gtd-organize)

(provide 'my-gtd-organize)
;;; my-gtd-organize.el ends here

;; Local Variables:
;; read-symbol-shorthands: (("mgo-" . "my-gtd-organize-"))
;; End:
