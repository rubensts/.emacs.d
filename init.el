;;; init.el --- Emacs configuration of Rubens Souza -*- lexical-binding: t; -*-
;;; Commentary:
;;; Code:


;; Added by Package.el.  This must come before configurations of
;; installed packages.  Don't delete this line.  If you don't want it,
;; just comment it out by adding a semicolon to the start of the line.
;; You may delete these explanatory comments.
;;(package-initialize)

(add-to-list 'load-path (expand-file-name
                         "lisp" user-emacs-directory))

;;----------------------------------------------------------------------------
;; Temporarily reduce garbage collection during startup
;; https://github.com/purcell/emacs.d/blob/master/init.el
;;----------------------------------------------------------------------------
(defconst my/initial-gc-cons-threshold gc-cons-threshold
  "Initial value of `gc-cons-threshold' at start-up time.")
(setq gc-cons-threshold (* 128 1024 1024))
(add-hook 'after-init-hook
          (lambda ()
            (setq gc-cons-threshold my/initial-gc-cons-threshold)))

(let ((file-name-handler-alist nil))

  ;;----------------------------------------------------------------------------
  ;; Load Cask - Package installation/dependencies are handled by cask & pallet
  ;;----------------------------------------------------------------------------
  (require 'cask "/usr/local/share/emacs/site-lisp/cask/cask.el")
  (cask-initialize)
  (require 'pallet)
  (pallet-mode t)

  ;;----------------------------------------------------------------------------
  ;; Set some default options
  ;;----------------------------------------------------------------------------
  (setq-default
   message-log-max 10000           ; increase size of *Messages* buffer
   load-prefer-newer t             ; avoid using outdated compiled files
   inhibit-default-init t          ; don't call default.el (default settings) after init.el
   package-enable-at-startup nil)  ; don't call package-initialize after init.el

  ;;----------------------------------------------------------------------------
  ;; Require use-package - loading packages is handled by it
  ;;----------------------------------------------------------------------------
  (require 'use-package)
  (setq-default use-package-always-defer t)

  ;;----------------------------------------------------------------------------
  ;; Load required packages
  ;;----------------------------------------------------------------------------
  (use-package s)  ; required library
  (use-package f)  ; required library

  (use-package delight)    ; to use :delight with use-package
  (use-package bind-key)   ; to use :bind (and variants) with use-package
  (use-package no-littering :demand t)   ; keeps ~/.emacs.d clean
  (use-package validate :demand t)
  (use-package org)

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

  (provide 'init))
;;; init.el ends here
