;;; -*- lexical-binding: t -*-

;;----------------------------------------------------------------------------
;; projectile
;;----------------------------------------------------------------------------
(use-package projectile
  :config
  (setq projectile-enable-caching t
        projectile-completion-system 'ivy
        projectile-switch-project-action 'projectile-dired
        projectile-mode-line '(:eval (format
                                      " :%s:" (projectile-project-name))))
  (projectile-mode))

(use-package ibuffer-projectile)


(use-package which-key
  :init (which-key-mode))

(use-package eyebrowse
  :config
  (setq eyebrowse-wrap-around t
        eyebrowse-new-workspace t
        eyebrowse-switch-back-and-forth t)
  (eyebrowse-mode t))

;; Version Control
;; magit
(use-package magit
  :hook (after-save . magit-after-save-refresh-status) ; refreshes magit status when file is saved
  :config
  (setq magit-completing-read-function 'ivy-completing-read
        magit-display-buffer-function 'magit-display-buffer-fullframe-status-topleft-v1)
  ;; automatically refreshes magit status after file is saved
  ;;(add-hook 'after-save-hook 'magit-after-save-refresh-status)
  )

;; git-gutter
(use-package git-gutter
  :init
  (global-git-gutter-mode +1))

;; git-timemachine
(use-package git-timemachine
  :commands git-timemachine)


;; Snippets
;; yasnippet
(use-package yasnippet
  :config
  (yas-global-mode 1))

(use-package yasnippet-snippets
  :after yasnippet)

;; yankpad
(use-package yankpad
  :config
  (setq yankpad-file "~/org/yankpad.org"))

;; recentf
(use-package recentf
  :config
  (recentf-mode t)
  (setq recentf-max-saved-items 10)

  (add-to-list 'recentf-exclude no-littering-var-directory)
  (add-to-list 'recentf-exclude no-littering-etc-directory)
  (add-to-list 'recentf-exclude '("COMMIT_MSG"
                                  "COMMIT_EDITMSG"
                                  "github.*txt$"
                                  ".*png$")))

;; restart-emacs
(use-package restart-emacs
 ;; :general (d/leader-keys "qr" 'restart-emacs)
)

;; ripgrep
(use-package rg)

;;----------------------------------------------------------------------------
;; Nicer naming of buffers for files with identical names
;;----------------------------------------------------------------------------
;; uniquify
(use-package uniquify
  :straight nil
  :config
  (setq uniquify-buffer-name-style 'reverse
        uniquify-separator " • "
        uniquify-after-kill-buffer-p t       ; rename after killing uniquified
        uniquify-ignore-buffers-re "^\\*"))  ; don't muck with special buffers

;; undo-tree
(use-package undo-tree
  ;;:chords (("uu" . undo-tree-visualize))
  :init
  (global-undo-tree-mode))

;; volatile-highlights
(use-package volatile-highlights
  :config
  (volatile-highlights-mode t))

;; anzu
(use-package anzu
  :bind (("M-%" . anzu-query-replace)
         ("C-M-%" . anzu-query-replace-regexp))
  :config
  (setq anzu-cons-mode-line-p nil
                 anzu-replace-to-string-separator "  ")
  (global-anzu-mode +1))

;; bookmarks
(use-package bookmark
  :config
  (setq bookmark-completion-ignore-case nil)
  (bookmark-maybe-load-default-file))

;; easy-kill
(use-package easy-kill
  :bind (([remap kill-ring-save] . easy-kill)
         ([remap mark-sexp] . easy-mark)))

;; ediff
(use-package ediff
  :hook ((ediff-before-setup . my-store-pre-ediff-winconfig)
         (ediff-quit . my-restore-pre-ediff-winconfig))
  :config
  (customize-set-variable 'ediff-window-setup-function 'ediff-setup-windows-plain)
  (customize-set-variable 'ediff-split-window-function 'split-window-horizontally)
  (customize-set-variable 'ediff-highlight-all-diffs 'nil) ; only highlight current diff
  (customize-set-variable 'ediff-diff-options "-w --text")

  ;; restore windows and layout after ending ediff session
  ;; https://emacs.stackexchange.com/questions/7482/restoring-windows-and-layout-after-an-ediff-session
  (defvar my-ediff-last-windows nil)

  (defun my-store-pre-ediff-winconfig ()
    (setq my-ediff-last-windows (current-window-configuration)))

  (defun my-restore-pre-ediff-winconfig ()
    (set-window-configuration my-ediff-last-windows)))

;; fill-column-indicator
(use-package fill-column-indicator
  :disabled t
  :config
  (setq fci-rule-width 1
                 fci-rule-color "#5d478b"
                 fci-rule-column 80)
  (define-globalized-minor-mode global-fci-mode fci-mode
    (lambda ()
      (fci-mode 1)))
  (global-fci-mode 1))


(provide 'base-extensions)
;;; base-extensions.el ends here
