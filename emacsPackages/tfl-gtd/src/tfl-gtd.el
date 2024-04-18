;;; tfl-gtd.el --- Project and task management with GTDish process  -*- lexical-binding: t; -*-
;;
;;  Copyright (c) 2024 Technosophist
;;
;;  Version: 0.0.0
;;
;;; Commentary:
;;
;; Project and task management with GTDish process.
;;
;;; Code:

(require 'use-package)
(use-package org
  :custom ((org-after-todo-state-change-hook '(tfl-gtd-add-delegatee-to-waiting
                                               tfl-gtd-add-created-to-todo))
           (org-log-done 'time)
           (org-log-into-drawer t)
           (org-log-states-order-reversed nil)
           (org-todo-keywords
            '((sequence "TODO(t!)" "WAIT(w@/!)" "|" "CANC(c@)" "DONE(d)")))
           (org-todo-state-tags-triggers
            '(("TODO" ("WAIT") ("CANC") ("DONE"))
              ("WAIT" ("WAIT" . t) ("HABIT") ("SOMEDAY") ("MAYBE") ("CANC") ("DONE"))
              ("CANC" ("WAIT") ("HABIT") ("SOMEDAY") ("MAYBE") ("CANC" . t) ("DONE"))
              ("DONE" ("WAIT") ("SOMEDAY") ("MAYBE") ("CANC") ("DONE" . t))
              ("" ("WAIT") ("HABIT") ("SOMEDAY") ("MAYBE") ("CANC") ("DONE")))))
  :defer)
(use-package org-faces
  :custom ((org-todo-keyword-faces '(("WAIT" . "blue") ("CANC" . "orange"))))
  :defer)
(use-package tfl-gtd-core
  :bind ("C-c g c" . tfl-gtd-capture)
  :hook (org-mode . tfl-gtd-set-regexp))
(use-package tfl-gtd-agenda
  :bind ("C-c g d" . tfl-gtd-agenda-daily))
(use-package tfl-gtd-process
  :bind ("C-c g p" . tfl-gtd-process))
(use-package tfl-gtd-review
  :bind ("C-c g r" . tfl-gtd-review))

(provide 'tfl-gtd)
;;; tfl-gtd.el ends here
