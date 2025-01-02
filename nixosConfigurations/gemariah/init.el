;;; init.el --- init script  -*- lexical-binding: t; -*-
;;
;;  Copyright (c) 2024 technosophist
;;
;;  Author: technosophist <technosophist@thoughtfull.systems>
;;
;;; Commentary:
;;
;;  None
;;
;;; Code:
(use-package org)
(use-package tfl-ol-obsidian)
(use-package tfl-org
  :after (tfl org))
(use-package tfl-org-bullets
  :after tfl-org)
(use-package tfl-org-capture
  :after tfl-org)
(use-package tfl-org-faces
  :after tfl-org)

(provide 'init)
;;; init.el ends here
