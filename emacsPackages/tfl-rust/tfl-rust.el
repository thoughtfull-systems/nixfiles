;;; tfl-rust.el --- Essentials for pleasant Rust development       -*- lexical-binding: t; -*-
;;
;; Copyright (c) 2025 technosophist
;;
;; Author: technosophist <technosophist@thoughtfull.systems>
;;
;;; Commentary:
;;
;; Configures essential of the Rust development experience like flycheck, etc.
;;
;;; Code:

(require 'tfl)

(use-package rust-mode)

(use-package flycheck-rust
  :hook (rust-mode . flycheck-rust-setup))

(provide 'tfl-rust)
;;; tfl-rust.el ends here
