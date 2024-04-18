;;; tfl-gtd-organize.el --- Organize GTD projects                        -*- lexical-binding: t; -*-

;;  Copyright (c) 2023 Technosophist

;;  Version: 0.0.0

;;; Commentary:

;; Take each unorganized project in a narrowed view for planning.

;;; Code:
(require 'tfl-gtd)
(require 'org-refile)
(require 'transient)

(defvar-keymap tgo-map
  :doc "Keymap for command `tfl-gtd-organize-mode', a minor mode."
  "C-c C-c" #'tgo-next)

(define-minor-mode tgo-mode
  "Minor mode for tfl-gtd-organize."
  :lighter " MGP"
  :keymap tgo-map
  :group 'tfl-gtd-organize
  (if tgo-mode
      (setq-local
       header-line-format
       `("Tasks should be SMART, singular, brief, verb-based!  "
         ,(substitute-command-keys "\\<tfl-gtd-organize-map>Finish `\\[tfl-gtd-organize-next]'.")))
    (setq-local header-line-format nil)))

(defvar tgo-buffer-name "*tfl-gtd-organize*")

;;;###autoload
(defun tfl-gtd-organize ()
  "Process each entry from inbox in a narrowed buffer for processing."
  (interactive)
  (if (get-buffer tgo-buffer-name)
      (pop-to-buffer tgo-buffer-name)
    (with-current-buffer
        (make-indirect-buffer (tfl-gtd-org-buffer tfl-gtd-todo-file-name) tgo-buffer-name t)
      (tgo-mode)
      (goto-char (point-min))
      (outline-next-heading)
      (tgo--next "No unorganized projects."))))

(defun tgo--next-heading-same-level ()
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
      (if (tfl-gtd-project-p)
          (if (tfl-gtd-active-p)
              (let ((point (save-excursion (mgp--find-unorganized-project))))
                (if (not point)
                    ;; organized project, keep looking
                    (setq continue (tgo--next-heading-same-level))
                  ;; unorganized project, exit early
                  (setq unorganized point)
                  (setq continue nil)))
            (unless (tgo--next-heading-same-level)
              (setq unorganized nil)
              (setq continue nil)))
        (if (or (not (org-get-todo-state))
                (not (tfl-gtd-active-p)))
            (unless (tgo--next-heading-same-level)
              (setq unorganized heading)
              (setq continue nil))
          (setq unorganized nil)
          (setq continue nil))))
    unorganized))

(defun tgo--first-unorganized-project ()
  (goto-char (point-min))
  (outline-next-heading)
  (let ((organized t)
        (continue t))
    (while (and organized continue)
      (if (save-excursion
            (outline-next-heading)
            (not (eq 2 (org-current-level))))
          (setq organized nil)
        (if (not (or (not (tfl-gtd-project-p))
                     (and (tfl-gtd-project-p)
                          (tfl-gtd-active-p)
                          (tgo--organized-project-p))))
            (setq organized nil)
          (setq continue (tgo--next-heading-same-level)))))
    organized))

(defun tgo--next (&optional msg)
  (if (not (tgo--next-unorganized-project))
      (progn
        (message (or msg "No more unorganized projects."))
        (kill-buffer tgo-buffer-name))
    (org-fold-heading nil t)
    (org-narrow-to-subtree)
    (pop-to-buffer tgo-buffer-name)))

(defun tgo-next ()
  "Move to next unorganized project for planning.
If there are no more unorganized projects, show MSG or a default."
  (interactive)
  (if (not (save-excursion (goto-char (point-min)) (tgo--organized-project-p)))
      (message "Project is not organized!")
    (widen)
    (goto-char (point-min))
    (tgo--next)))

;; (deftheme tfl-gtd-organize)
;; (custom-theme-set-variables
;;  'tfl-gtd-organize
;;  )
;; (provide-theme 'tfl-gtd-organize)
;; (enable-theme 'tfl-gtd-organize)

(provide 'tfl-gtd-organize)
;;; tfl-gtd-organize.el ends here

;; Local Variables:
;; read-symbol-shorthands: (("tgo-" . "tfl-gtd-organize-"))
;; End:
