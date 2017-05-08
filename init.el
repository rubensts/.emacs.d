;;; init.el --- My Emacs configuration

;; This program is free software. It comes without any warranty, to the extent
;; permitted by applicable law. You can redistribute it and/or modify it under the
;; terms of the Do What The Fuck You Want To Public License, Version 2, as published
;; by Sam Hocevar. See http://www.wtfpl.net/ for more details.

;;; Commentary:

;; Following lines load an Org file and build the configuration code out of it. This
;; process is known as tangling.
;;
;; See README.org for more details.

;;; Code:

;; Increase Garbage Collector during the load up, so things go a little snappier
(let ((gc-cons-threshold most-positive-fixnum))

  ;; Alert in case Emacs version < 25
  (when (version< emacs-version "25")
    (warn "This configuration needs Emacs 25, but this is %s!" emacs-version))

  ;; Loading it here allows adding the new repositories listed below
  (require 'package)

  (setq-default
   message-log-max 10000           ; increase size of *Messages* buffer
   load-prefer-newer t             ; avoid using outdated compiled files
   inhibit-default-init t          ; don't call default.el (default settings) after init.el
   package-enable-at-startup nil)  ; don't call package-initialize again after init.el

  ;; Sets the ELPA repositories from where packages are fetched
  (setq package-archives '(("org"   . "http://orgmode.org/elpa/")
                           ("melpa" . "https://melpa.org/packages/")
                           ("gnu"   . "https://elpa.gnu.org/packages/")))

  ;; By default package-initialize is called after init.el.
  ;; Calling it here because some packages listed depend on it.
  (package-initialize)

  ;; Install dependencies
  (unless (and (package-installed-p 'delight)
               (package-installed-p 'use-package))
    (package-refresh-contents)
    (package-install 'delight t)
    (package-install 'use-package t))

  (eval-when-compile
    (require 'use-package)
    (setq-default use-package-always-defer t
                  use-package-always-ensure t))

  (require 'subr-x)
  (require 'time-date)

  ;; Help keeping ~/.emacs.d clean
  (use-package no-littering :demand t)

  (use-package validate :demand t)

  ;; load libraries
  (use-package s)
  (use-package f)

  ;; exec-path-from-shell
  ;; Library to ensure environment variables inside Emacs look the same
  ;; as in the user's shell.
  (use-package exec-path-from-shell
    :if (display-graphic-p)
    :config
    (progn
      (when (string-match-p "/zsh$" (getenv "SHELL"))
        ;; Use a non-interactive login shell.  A login shell, because my
        ;; environment variables are mostly set in `.zprofile'.
        (setq exec-path-from-shell-arguments '("-l")))

      ;; Import additional environment variables beyond just $PATH
      (dolist (var '("PYTHONPATH"         ; Python modules
                     "INFOPATH"           ; Info directories
                     ))
        (add-to-list 'exec-path-from-shell-variables var))

      ;; Initialize Emacs' environment from the shell
      (exec-path-from-shell-initialize)))

  ;; Use latest Org
  (use-package org
    :pin org
    :ensure org-plus-contrib)

  ;; Tangle configuration
  (org-babel-load-file (expand-file-name "emacs-init.org"
                                         user-emacs-directory))

  ;;; Initialize Emacs server
  (use-package server
    :config
    (unless (server-running-p) (server-start)))

  (garbage-collect))

;;; init.el ends here
