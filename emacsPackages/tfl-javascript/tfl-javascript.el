;;; tfl-javascript.el --- JavaScript development configuration           -*- lexical-binding: t; -*-

;;  Copyright (c) 2023 Paul Stadig

;;  Version: 0.0.0

;;; Commentary:

;; None

;;; Code:

(use-package js2-mode
  :mode "\\.js\\'")
(use-package json-mode)
(use-package typescript-mode)

;; (deftheme tfl-javascript)
;; (custom-theme-set-variables
;;  'tfl-javascript
;;  )
;; (provide-theme 'tfl-javascript)
;; (enable-theme 'tfl-javascript)
(provide 'tfl-javascript)
;;; tfl-javascript.el ends here
