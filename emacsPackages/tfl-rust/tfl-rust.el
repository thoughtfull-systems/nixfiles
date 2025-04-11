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

(use-package flycheck-rust
  :hook (rust-mode . flycheck-rust-setup))
(use-package lsp-mode
  :custom ((lsp-rust-analyzer-cargo-watch-command "clippy")
           (lsp-rust-analyzer-display-lifetime-elision-hints-enable "skip_trivial")
           (lsp-rust-analyzer-display-chaining-hints t)
           (lsp-rust-analyzer-display-lifetime-elision-hints-use-parameter-names nil)
           (lsp-rust-analyzer-display-closure-return-type-hints t)
           (lsp-rust-analyzer-display-parameter-hints nil)
           (lsp-rust-analyzer-display-reborrow-hints nil)))
(use-package rust-mode
  :defer)
(use-package rustic
  :defer)

(provide 'tfl-rust)
;;; tfl-rust.el ends here
