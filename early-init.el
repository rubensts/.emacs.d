;;; early-init.el -*- lexical-binding: t; -*-

;; Emacs HEAD (27+) introduces early-init.el, which is run before init.el,
;; before package and UI initialization happens. We can use this opportunity to
;; cull parts of the startup process early and optimize Emacs startup ASAP.

;; Before Emacs 27, the init file was responsible for initializing the package
;; manager by calling `package-initialize'. Emacs 27 changed the default
;; behavior: It now calls `package-initialize' before loading the init
;; file. However, Emacs 27 also loads the "early init" file (this file) before
;; it initializes the package manager, allowing to use this early init file to
;; prevent Emacs from initializing the package manager.

;;----------------------------------------------------------------------------
;; Package initialize occurs automatically, before `user-init-file' is
;; loaded, but after `early-init-file'. I prefer to handle packages
;; installation through straight.el, so we must prevent Emacs from
;; doing it early!
;; ----------------------------------------------------------------------------
(setq package-enable-at-startup nil)

;; Prevent the glimpse of un-styled Emacs by setting these early.
(add-to-list 'default-frame-alist '(tool-bar-lines . 0))
(add-to-list 'default-frame-alist '(menu-bar-lines . 0))
(add-to-list 'default-frame-alist '(vertical-scroll-bars))
