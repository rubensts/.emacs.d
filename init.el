;;; init.el --- Emacs configuration of Rubens Souza -*- lexical-binding: t; -*-
;;; Commentary:
;;; Code:

;; straight.el bootstrap
(let ((bootstrap-file (concat user-emacs-directory "straight/repos/straight.el/bootstrap.el"))
      (bootstrap-version 3))
  (unless (file-exists-p bootstrap-file)
    (with-current-buffer
        (url-retrieve-synchronously
         "https://raw.githubusercontent.com/raxod502/straight.el/develop/install.el"
         'silent 'inhibit-cookies)
      (goto-char (point-max))
      (eval-print-last-sexp)))
  (load bootstrap-file nil 'nomessage))

(setq straight-use-package-by-default t)

;;----------------------------------------------------------------------------
;; Increase memory to 20MB before calling garbage collector, to make things
;; snappier (default is 800KB)
;;----------------------------------------------------------------------------
(setq gc-cons-threshold (* 20 1024 1024))

;;----------------------------------------------------------------------------
;; Require use-package - loading packages is handled by it
;;----------------------------------------------------------------------------
(straight-use-package 'use-package)
(setq-default use-package-always-defer t)

;;----------------------------------------------------------------------------
;; Load required packages
;;----------------------------------------------------------------------------
;; keeps ~/.emacs.d clean
(use-package no-littering
  :demand t
  :config
  (setq auto-save-file-name-transforms
        `((".*" ,(no-littering-expand-var-file-name "auto-save/") t)))
  (setq custom-file (no-littering-expand-etc-file-name "custom.el")))

(use-package s)  ; required library
(use-package f)  ; required library
(use-package delight)    ; to use :delight with use-package
(use-package bind-key)   ; to use :bind (and variants) with use-package
(use-package validate :demand t)
(use-package org-plus-contrib)

;;----------------------------------------------------------------------------
;; Load (tangle) rest of the configuration through org - Literate Programming
;;
;; org-babel-load-file increases startup time, so only do it if necessary.
;; To reload any config changes, delete emacs-init.el and restart emacs.
;;----------------------------------------------------------------------------
(if (file-exists-p (expand-file-name "emacs-init.el" user-emacs-directory))
    (load-file (expand-file-name "emacs-init.el" user-emacs-directory))
  ;; else
  (org-babel-load-file (expand-file-name "readme.org" user-emacs-directory)))

;;----------------------------------------------------------------------------
;; Initialize Emacs server - allow access from emacsclient
;;----------------------------------------------------------------------------
(use-package server
  :demand t
  :config
  (unless (server-running-p)
    (server-start)))

(provide 'init)
;;; init.el ends here
