;;; base.el --- Base configuration -*- lexical-binding: t -*-

(load custom-file 'no-error 'no-message)

;; Core settings
;; make sure that UTF-8 is used everywhere
(set-charset-priority 'unicode)
(setq locale-coding-system   'utf-8)
(set-language-environment    'utf-8)
(set-terminal-coding-system  'utf-8)
(set-keyboard-coding-system  'utf-8)
(set-default-coding-systems  'utf-8)
(set-selection-coding-system 'utf-8)
(prefer-coding-system        'utf-8)
(set-input-method nil)

;; Emacs customizations
(fset 'yes-or-no-p 'y-or-n-p)           ; ask `y/n?` instead of `yes/no?`
(transient-mark-mode t)                 ; apply changes to highlighted region
(delete-selection-mode t)               ; overwrite selected text when typing
(blink-cursor-mode -1)                  ; turn off the blinking cursor
(global-font-lock-mode t)               ; always highlight code
(global-auto-revert-mode t)             ; refresh buffers when files change
(global-hl-line-mode 1)                 ; highlight the current line
(global-visual-line-mode t)             ; break long line of text
(global-prettify-symbols-mode 1)        ; prettify symbols (lambdas, etc)
(column-number-mode t)                  ; shows column number on the modeline
(save-place-mode t)                     ; save cursor position for opened files
(show-paren-mode t)                     ; show matching parentheses
(winner-mode 1)                         ; get back to previous window configuration

(defalias 'list-buffers 'ibuffer)       ; use ibuffer by default

(setq load-prefer-newer t             ; avoid using outdated compiled files
      message-log-max 16384           ; increase messages buffer size
      inhibit-default-init t          ; don't call default.el after init.el
      inhibit-startup-message t       ; don't show startup message
      initial-major-mode 'org-mode    ; set scratch buffer automatically to org-mode
      initial-scratch-message nil     ; clear the inital message on the scratch buffer
      mouse-yank-at-point t           ; paste from clipboard to where point is on buffer
      echo-keystrokes 0.1             ; shows keystrokes in progress
      use-dialog-box nil              ; don't use dialog when using mouse click
      line-spacing '0.10              ; line height
      default-directory "~/"          ; start searching from home directory when opening files
      vc-follow-symlinks t            ; when opening a file, always follow symlinks
      sentence-end-double-space nil   ; a sentence shouldn't have two spaces after period
      require-final-newline t         ; ensure files end with newline
      confirm-kill-emacs 'y-or-n-p    ; ask for confirmation to close Emacs
      show-paren-delay 0.0            ; set delay to 0 for showing matching parens
      auto-window-vscroll nil         ; reduce lag when using next-line
      ;;apropos-do-all t                ; perform more extensive searches than default
      fringes-outside-margins t
      cursor-in-non-selected-windows nil     ; disable cursor on non select window
      save-interprogram-paste-before-kill t  ; save clipboard to kill-ring before deleting it

      ;; http://ergoemacs.org/emacs/emacs_stop_cursor_enter_prompt.html
      minibuffer-prompt-properties
      '(read-only t point-entered minibuffer-avoid-prompt face minibuffer-prompt))

(setq-default indicate-empty-lines t)          ; show empty lines at bottom of buffer
(setq-default ffap-machine-p-known 'reject)    ; stop pinging the host at point when C-x C-f a file

(setq-default indent-tabs-mode nil             ; always indent with spaces
              default-tab-width 2
              c-basic-offset 4
              fill-column 90)                  ; set default line length

;;; hooks
;; delete trailing whitespace when buffer is saved
(add-hook 'before-save-hook 'delete-trailing-whitespace)
;; treat CamelCaseSubWords as separate words
(add-hook 'prog-mode-hook 'subword-mode)
;; when saving a file that starts with `#!', make it executable
(add-hook 'after-save-hook
          'executable-make-buffer-file-executable-if-script-p)

;; Location
(setq calendar-week-start-day  1
      calendar-latitude 43.8
      calendar-longitude 11.0
      calendar-location-name "Prato, Italy")

;; Holidays
(setq holiday-general-holidays
      '((holiday-fixed 1 1 "Capodanno")
        (holiday-fixed 5 1 "1 Maggio")
        (holiday-fixed 4 25 "Liberazione")
        (holiday-fixed 6 2 "Festa Repubblica")
        ))

(setq holiday-christian-holidays
      '((holiday-fixed 12 8 "Immacolata Concezione")
        (holiday-fixed 12 25 "Natale")
        (holiday-fixed 12 26 "Santo Stefano")
        (holiday-fixed 1 6 "Epifania")
        (holiday-easter-etc -52 "Giovedì grasso")
        (holiday-easter-etc -47 "Martedì grasso")
        (holiday-easter-etc  -2 "Venerdì Santo")
        (holiday-easter-etc   0 "Pasqua")
        (holiday-easter-etc  +1 "Lunedì Pasqua")
        (holiday-fixed 8 15 "Assunzione di Maria")
        (holiday-fixed 11 1 "Ognissanti")
        ))

;; History
(setq-default history-length 1000
              history-delete-duplicates t
              savehist-save-minibuffer-history 1
              savehist-additional-variables '(kill-ring
                                              search-ring
                                              regexp-search-ring))
(savehist-mode t)

;; Scrolling
(setq scroll-margin 0
      scroll-conservatively 100000
      scroll-preserve-screen-position 1
      mouse-wheel-scroll-amount '(1 ((shift) . 1))
      mouse-wheel-progressive-speed nil
      mouse-wheel-follow-mouse 't)

;; Useful functions
(put 'downcase-region 'disabled nil)
(put 'upcase-region 'disabled nil)
(put 'narrow-to-region 'disabled nil)
(put 'narrow-to-page 'disabled nil)

(provide 'base)
;;; base.el ends here
