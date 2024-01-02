;;; early-init.el --- Early initialization                                -*- lexical-binding: t -*-
;;
;; Copyright (c) 2024 Paul Stadig
;;
;; Author: Paul Stadig <paul@thoughtfull.systems>
;;
;;; Commentary:
;;
;; Emacs initialization prior to package and GUI initialization (see
;; https://www.gnu.org/software/emacs/manual/html_node/emacs/Early-Init-File.html#Early-Init-File).
;;
;; `custom-file' is set and loaded here.  This must be done early on otherwise if code modifies a
;; custom variable, then init.el will be modified, but modifying init.el fails because it is in the
;; nix store and marked read-only.  An alternative is carefully specifying library initialization
;; order, but that is a fragile solution that has broken several times.
;;
;;; Code:

;; This needs to be done as early as possible.
(setq custom-file "~/.config/emacs/custom.el")
(load custom-file 'noerror 'nomessage 'nosuffix)

(provide 'early-init)
;;; early-init.el ends here
