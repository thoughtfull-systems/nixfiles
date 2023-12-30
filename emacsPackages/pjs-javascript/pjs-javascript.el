;;; pjs-javascript.el --- JavaScript development configuration           -*- lexical-binding: t; -*-

;;  Copyright (c) 2023 Paul Stadig

;;  Version: 0.0.0

;;; Commentary:

;; None

;;; Code:

(use-package js2-mode
  :mode "\\.js\\'")
(use-package json-mode)
(use-package typescript-mode)

;; (deftheme pjs-javascript)
;; (custom-theme-set-variables
;;  'pjs-javascript
;;  )
;; (provide-theme 'pjs-javascript)
;; (enable-theme 'pjs-javascript)
(provide 'pjs-javascript)
;;; pjs-javascript.el ends here
