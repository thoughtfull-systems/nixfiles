;;; tfl-gtd-review.el --- Weekly review                                  -*- lexical-binding: t; -*-

;;  Copyright (c) 2024 Paul Stadig

;;  Version: 0.0.0

;;; Commentary:

;; Weekly review agenda with waiting, someday, maybe, and stuck project sections

;;; Code:
(require 'tfl-gtd)
(require 'org-agenda)

(defvar tgr--buffer-name "*tfl-gtd-review*")
(defvar tgr--buffer nil)

;;;###autoload
(defun tfl-gtd-review ()
  "Weekly review agenda with waiting, someday, maybe, and stuck project sections."
  (interactive)
  (if (buffer-live-p tgr--buffer)
      (switch-to-buffer tgr--buffer)
    (let* ((org-agenda-custom-commands
            `(("z" "Weekly review"
               ((tags "WAIT" ((org-agenda-overriding-header "Waiting tasks")))
                (tags "SOMEDAY" ((org-agenda-overriding-header "Someday tasks")))
                (tags "MAYBE" ((org-agenda-overriding-header "Maybe tasks"))))
               ((org-agenda-prefix-format
                 '((agenda . " %i %?-12t% s%b")
                   (todo . " %i %b")
                   (tags . " %i %b")
                   (search . " %i %b")))
                (org-agenda-tag-filter-preset
                 '("-ARCHIVE" "-DONE" "-CANC"))
                (org-agenda-window-setup 'current-window))))))
      (org-agenda nil "z")
      (setq tgr--buffer (current-buffer)))))

;; (deftheme tfl-gtd-review)
;; (custom-theme-set-variables
;;  'tfl-gtd-review
;;  )
;; (provide-theme 'tfl-gtd-review)
;; (enable-theme 'tfl-gtd-review)

(provide 'tfl-gtd-review)
;;; tfl-gtd-review.el ends here

;; Local Variables:
;; read-symbol-shorthands: (("tgr-" . "tfl-gtd-review-"))
;; End:
