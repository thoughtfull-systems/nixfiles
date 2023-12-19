;;; my-javascript.el --- JavaScript development configuration  -*- lexical-binding: t; -*-

;;  Copyright (c) 2023 Paul Stadig

;;  Version: 0.0.0

;;; Commentary:

;; None

;;; Code:

(use-package my-prog)
(use-package js2-mode
  :mode "\\.js\\'")
(use-package json-mode)
(use-package typescript-mode)

;; (deftheme my-javascript)
;; (custom-theme-set-variables
;;  'my-javascript
;;  )
;; (provide-theme 'my-javascript)
;; (enable-theme 'my-javascript)
(provide 'my-javascript)
;;; my-javascript.el ends here
