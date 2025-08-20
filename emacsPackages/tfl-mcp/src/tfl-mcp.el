;;; tfl-mcp.el --- Setup MCP                                             -*- lexical-binding: t; -*-
;;
;; Copyright (c) 2025 technosophist
;;
;; Author: technosophist <technosophist@thoughtfull.systems>
;;
;;; Commentary:
;;
;; Configures essential of the MCP for Clojure.
;;
;;; Code:

(use-package eca)
(use-package gptel
  :config
  (require 'gptel-integrations))
(use-package mcp)

(provide 'tfl-mcp)
;;; tfl-mcp.el ends here
