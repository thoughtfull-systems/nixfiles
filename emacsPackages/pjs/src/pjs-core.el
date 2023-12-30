;;; pjs-core.el --- Shared definitions                                    -*- lexical-binding: t -*-
;;
;; Copyright (c) 2023 Paul Stadig
;;
;; Author: Paul Stadig <paul@thoughtfull.systems>
;;

;;; Commentary:
;;
;; Shared definitions separated to prevent circular dependencies.
;;

;;; Code:
(defmacro pjs-with-theme (theme &rest body)
  "Create and enable THEME, if it doesn't exist.
BODY is presumed to be `custom-theme-set-variables' and `custom-theme-set-faces'
forms.

A package can configure variables and faces itself, instead of configuration
being done in `use-package' when requiring the package.

Using a theme allows the user to override by using customization.  After
creating the theme, remove it from `custom-enabled-themes', so it won't be
disabled when iterating over all themes to disable them.

This technique is borrowed from `use-package'."
  (declare (indent 1))
  `(let
       ((custom--inhibit-theme-enable nil))
     (unless (memq ,theme custom-known-themes)
       (deftheme ,(cadr theme))
       (enable-theme ,theme)
       ;; From use-package: Remove the synthetic theme from the enabled themes, so iterating over
       ;; them to "disable all themes" won't disable it.
       (setq custom-enabled-themes
             (remq ,theme custom-enabled-themes)))
     ,@body))

(defmacro pjs-custom-theme-set-variables (theme &rest variables)
  "Create THEME with VARIABLES or add VARIABLES to existing THEME.
VARIABLES are lists of the form (<symbol> <value> [comment]).

A package can configure variables itself, instead of configuration being done in
`use-package' when requiring the package.

Using a theme allows the user to override by using customization.  After
creating the theme, remove it from `custom-enabled-themes', so it won't be
disabled when iterating over all themes to disable them.

This technique is borrowed from `use-package'."
  `(pjs-with-theme ,theme
     (custom-theme-set-variables ,theme ,@variables)))

(defmacro pjs-custom-theme-set-faces (theme &rest faces)
  "Create THEME with FACES or add FACES to existing THEME.
FACES are lists of the form (<symbol> <face-spec>).

A package can configure faces itself, instead of configuration being done in
`use-package' when requiring the package.

Using a theme allows the user to override by using customization.  After
creating the theme, remove it from `custom-enabled-themes', so it won't be
disabled when iterating over all themes to disable them.

This technique is borrowed from `use-package'."
  `(pjs-with-theme ,theme
     (custom-theme-set-faces ,theme ,@faces)))

(provide 'pjs-core)
;;; pjs-core.el ends here
