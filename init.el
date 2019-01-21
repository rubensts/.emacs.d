;;; init.el --- Load the full configuration -*- lexical-binding: t -*-
;;; Commentary:
;; This file bootstraps the configuration, which is divided into a number of other files.

;; Code:
;;; Improving speed
;;;; Garbage collection
;; Adjust garbage collection thresholds during startup, and thereafter
(let ((normal-gc-cons-threshold (* 20 1024 1024))
      (init-gc-cons-threshold (* 128 1024 1024)))
  (setq gc-cons-threshold init-gc-cons-threshold)
  (add-hook 'emacs-startup-hook
	    (lambda () (setq gc-cons-threshold normal-gc-cons-threshold))))

;;;; File name handler
;; To also help speeding startup, temporarily disable the file name handler.
(defvar original-file-name-handler-alist file-name-handler-alist)
(setq file-name-handler-alist nil)
(add-hook 'emacs-startup-hook
	  (lambda () (setq file-name-handler-alist original-file-name-handler-alist)))

;;; Straight.el
;; Packages managemente is handled by straight.el
;;;; Bootstrap
(defvar bootstrap-version)
(let ((bootstrap-file
       (expand-file-name "straight/repos/straight.el/bootstrap.el" user-emacs-directory))
      (bootstrap-version 5))
  (unless (file-exists-p bootstrap-file)
    (with-current-buffer
        (url-retrieve-synchronously
         "https://raw.githubusercontent.com/raxod502/straight.el/develop/install.el"
         'silent 'inhibit-cookies)
      (goto-char (point-max))
      (eval-print-last-sexp)))
  (load bootstrap-file nil 'nomessage))

;;;; Integration with use-package
(straight-use-package 'use-package)
(setq straight-use-package-by-default t)

;;;; Fix issue when installing org though straight.el
;; https://github.com/raxod502/straight.el#installing-org-with-straightel
(require 'subr-x)
(straight-use-package 'git)

(defun org-git-version ()
  "The Git version of org-mode.
Inserted by installing org-mode or when a release is made."
  (require 'git)
  (let ((git-repo (expand-file-name
                   "straight/repos/org/" user-emacs-directory)))
    (string-trim
     (git-run "describe"
              "--match=release\*"
              "--abbrev=6"
              "HEAD"))))

(defun org-release ()
  "The release version of org-mode.
Inserted by installing org-mode or when a release is made."
  (require 'git)
  (let ((git-repo (expand-file-name
                   "straight/repos/org/" user-emacs-directory)))
    (string-trim
     (string-remove-prefix
      "release_"
      (git-run "describe"
               "--match=release\*"
               "--abbrev=0"
               "HEAD")))))

(provide 'org-version)

;;; Initial packages
;;;; Load libraries
(require 'time-date)
(use-package s)
(use-package f)

;;; No-littering
;; Help keeping ~/.emacs.d clean
(use-package no-littering
  :config
  ;; handle also auto-save files
  (setq auto-save-file-name-transforms
      `((".*" ,(no-littering-expand-var-file-name "auto-save/") t)))
  ;; save custom file also on littering directory
  (setq custom-file (no-littering-expand-etc-file-name "custom.el")))

;;; Bootstrap config
;; Store additional config in the 'elisp' subfolder and add it to the load path
;; so that `require' can find the files.
(add-to-list 'load-path (expand-file-name "elisp" user-emacs-directory))

(require 'base)
(require 'base-functions)
(require 'base-os)
(require 'ext-ivy)
(require 'ext-org)
(require 'base-extensions)
(require 'ext-company)
(require 'ext-magit)
(require 'ext-treemacs)
(require 'ext-devops)
(require 'lang-python)
(require 'appearance-theme)
(require 'appearance-fonts)
(require 'appearance-modeline)
