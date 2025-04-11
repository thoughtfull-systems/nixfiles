;; This needs to be done as early as possible.
(setq custom-file "~/.config/emacs/custom.el")
(load custom-file 'noerror 'nomessage 'nosuffix)

(setq gc-cons-threshold 100000000)
(setq read-process-output-max (* 1024 1024))

;; Local Variables:
;; flycheck-disabled-checkers: (emacs-lisp-checkdoc)
;; End:
