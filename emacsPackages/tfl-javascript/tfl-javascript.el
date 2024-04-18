;;; tfl-javascript.el --- JavaScript development configuration           -*- lexical-binding: t; -*-
;;
;; Copyright (c) 2024 Technosophist
;;
;; Author: Technosophist <technosophist@thoughtfull.systems>
;;
;;; Commentary:
;;
;; Configure js2, json, and typescript modes.
;;
;;; Code:

(use-package js2-mode
  :mode "\\.js\\'")
(use-package json-mode)
(use-package typescript-mode)

(provide 'tfl-javascript)
;;; tfl-javascript.el ends here
