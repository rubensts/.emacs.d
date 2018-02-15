;;; init.el --- Emacs configuration of Rubens Souza -*- lexical-binding: t; -*-
;;; Commentary:
;;; Code:

;; Loading it here allows adding the new repositories listed below
(require 'package)

;; Sets the ELPA repositories from where packages are fetched
(setq package-archives '(("org"   . "https://orgmode.org/elpa/")
                         ("melpa" . "https://melpa.org/packages/")
                         ("gnu"   . "https://elpa.gnu.org/packages/")))

;; By default package-initialize is called after init.el.
;; Calling it here because some packages listed depend on it.
(package-initialize)

;;----------------------------------------------------------------------------
;; Increase memory to 50MB before calling garbage collector, to make things
;; snappier (default is 800KB)
;;----------------------------------------------------------------------------
(setq gc-cons-threshold (* 50 1024 1024))

;;----------------------------------------------------------------------------
;; Require use-package - loading packages is handled by it
;;----------------------------------------------------------------------------
(unless (package-installed-p 'use-package)
  (package-refresh-contents)
  (package-install 'use-package))

(eval-when-compile
  (require 'use-package)
  (setq-default use-package-always-defer t
                use-package-always-ensure t))

(require 'subr-x)
(require 'time-date)

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
