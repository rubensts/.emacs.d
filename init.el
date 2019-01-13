;;; -*- lexical-binding: t -*-

;; [[file:~/.emacs.d/readme.org::*Disable%20package.el][Disable package.el:1]]
;;(setq package-enable-at-startup nil)
;;(require 'package)
;; Disable package.el:1 ends here

;; [[file:~/.emacs.d/readme.org::*Repositories][Repositories:1]]
(setq package-archives '(("org"   . "https://orgmode.org/elpa/")
                         ("melpa" . "https://melpa.org/packages/")
                         ("gnu"   . "https://elpa.gnu.org/packages/")))
(package-initialize)
;; Repositories:1 ends here

;; [[file:~/.emacs.d/readme.org::*Garbage%20collector][Garbage collector:1]]
;;(let ((my/file-name-handler-alist file-name-handler-alist))
;;
;;  (setq gc-cons-threshold (* 400 1024 1024)
;;        file-name-handler-alist nil)
;;
;;  ;; make sure to reset the variables after init.
;;  (add-hook 'after-init-hook
;;            (lambda ()
;;              (setq gc-cons-threshold (* 20 1024 1024)
;;                    file-name-handler-alist my/file-name-handler-alist))))
;; Garbage collector:1 ends here

;; [[file:~/.emacs.d/readme.org::*Garbage%20collector][Garbage collector:2]]
;;(defun my-minibuffer-setup-hook ()
;;  (setq gc-cons-threshold most-positive-fixnum))
;;
;;(defun my-minibuffer-exit-hook ()
;;  (setq gc-cons-threshold (* 20 1024 1024)))
;;
;;(add-hook 'minibuffer-setup-hook #'my-minibuffer-setup-hook)
;;(add-hook 'minibuffer-exit-hook #'my-minibuffer-exit-hook)
;; Garbage collector:2 ends here

;; [[file:~/.emacs.d/readme.org::*Package%20management][Package management:1]]
  (unless (package-installed-p 'use-package)
    (package-refresh-contents)
    (package-install 'use-package))

  (eval-when-compile
    (require 'use-package)
    (setq-default use-package-always-ensure t))

  (use-package use-package-chords
    :config (key-chord-mode 1))
;; Package management:1 ends here

;; [[file:~/.emacs.d/readme.org::*Server][Server:1]]
(use-package server
  :config
  (unless (server-running-p)
    (server-start)))
;; Server:1 ends here

;; [[file:~/.emacs.d/readme.org::*Packages][Packages:1]]
;; keeps ~/.emacs.d clean
(use-package no-littering
  :config
  (setq auto-save-file-name-transforms
        `((".*" ,(no-littering-expand-var-file-name "auto-save/") t)))
  (setq custom-file (no-littering-expand-etc-file-name "custom.el")))

(use-package s)  ; required library
(use-package f)  ; required library
(use-package bind-key)   ; to use :bind (and variants) with use-package
(use-package validate)
;; Packages:1 ends here

;; [[file:~/.emacs.d/readme.org::*Generate%20init.el][Generate init.el:1]]
(defun my/tangle-init ()
  "Tangles readme.org and byte compiles ~/.emacs.d/"
  (interactive)
  (when (equal (buffer-name)
               (concat "readme.org"))
    (org-babel-tangle)
    ;; byte compile the .emacs.d directory
    ;;(byte-recompile-directory (expand-file-name user-emacs-directory) 0)
    ))

(add-hook 'after-save-hook 'my/tangle-init)
;;(add-hook 'kill-emacs-hook 'my/tangle-init)
;; Generate init.el:1 ends here

;; [[file:~/.emacs.d/readme.org::*OS%20compatibility][OS compatibility:1]]
 (defun system-is-mac ()
   (interactive)
   (string-equal system-type "darwin"))

 (defun system-is-linux ()
   (interactive)
   (string-equal system-type "gnu/linux"))
;; OS compatibility:1 ends here

;; [[file:~/.emacs.d/readme.org::*Better%20defaults][Better defaults:1]]
;; Fullscreen
(cond ((system-is-mac) (toggle-frame-fullscreen))
      ((system-is-linux) (add-to-list 'default-frame-alist
                                        '(fullscreen . maximized))))

;; better defaults
;;(menu-bar-mode -1)
;;(when (fboundp 'tool-bar-mode)
;;  (tool-bar-mode -1))
;;(when (fboundp 'scroll-bar-mode)
;;  (scroll-bar-mode -1))
;;(when (fboundp 'horizontal-scroll-bar-mode)
;;  (horizontal-scroll-bar-mode -1))
;;
;;(global-set-key (kbd "M-/") 'hippie-expand)
;;(global-set-key (kbd "C-x C-b") 'ibuffer)

(fset 'yes-or-no-p 'y-or-n-p)                  ; ask `y/n?` instead of `yes/no?`
(transient-mark-mode t)                        ; apply changes to highlighted region
(delete-selection-mode t)                      ; overwrite selected text when typing
(blink-cursor-mode -1)                         ; turn off the blinking cursor
(global-font-lock-mode t)                      ; always highlight code
(global-auto-revert-mode t)                    ; refresh buffers when files change
(global-hl-line-mode 1)                        ; highlight the current line
(global-visual-line-mode t)                    ; break long line of text
(global-prettify-symbols-mode 1)               ; prettify symbols (lambdas, etc)
(column-number-mode t)                         ; shows column number on the modeline
(save-place-mode t)                            ; save cursor position for opened files
(show-paren-mode t)                            ; show matching parentheses
(winner-mode 1)                                ; allow to get back to previous window configuration

(defalias 'list-buffers 'ibuffer)              ; use ibuffer by default

(set-terminal-coding-system  'utf-8)           ; make sure that UTF-8 is used everywhere
(set-keyboard-coding-system  'utf-8)
;;(set-language-environment    'utf-8)
(set-language-environment   "UTF-8")
(set-default-coding-systems  'utf-8)
(set-selection-coding-system 'utf-8)
(setq locale-coding-system   'utf-8)
(prefer-coding-system        'utf-8)
(set-input-method nil)

(validate-setq load-prefer-newer t             ; avoid using outdated compiled files
               inhibit-default-init t          ; don't call default.el (default settings) after init.el
               inhibit-startup-message t       ; don't show startup message
               initial-major-mode 'org-mode    ; set scratch buffer automatically to org-mode
               initial-scratch-message nil     ; clear the inital message on the scratch buffer

               mouse-yank-at-point t           ; paste from clipboard to where point is on buffer
               echo-keystrokes 0.1             ; shows keystrokes in progress
               use-dialog-box nil              ; don't use dialog when using mouse click
               line-spacing '0.10              ; line height
               ;;auto-revert-verbose nil         ; turn off auto revert messages in the minibuffer
               default-directory "~/"          ; start searching from home directory when opening files
               vc-follow-symlinks t            ; when opening a file, always follow symlinks
               sentence-end-double-space nil   ; a sentence shouldn't have two spaces after period
               require-final-newline t         ; ensure files end with newline
               confirm-kill-emacs 'y-or-n-p    ; ask for confirmation to close Emacs
               auto-revert-verbose nil         ; turn off auto revert messages in the minibuffer
               ;;size-indication-mode t          ; displays the buffer size in the modeline
               show-paren-delay 0.0            ; set delay to 0 for showing matching parens
               auto-window-vscroll nil         ; reduce lag when using next-line
               save-interprogram-paste-before-kill t)

(setq apropos-do-all t)                        ; perform more extensive searches than default
(setq-default indicate-empty-lines t)          ; show empty lines at bottom of buffer
(setq-default ffap-machine-p-known 'reject)    ; stop pinging the host at point when C-x C-f a file

(setq-default indent-tabs-mode nil             ; always indent with spaces
              default-tab-width 2
              c-basic-offset 4
              fill-column 90)                  ; set default line length

;;; hooks
(add-hook 'before-save-hook 'delete-trailing-whitespace)  ; delete trailing whitespace when buffer is saved
(add-hook 'prog-mode-hook 'subword-mode)                  ; treat CamelCaseSubWords as separate words

;; when saving a file that starts with `#!', make it executable
(add-hook 'after-save-hook
          'executable-make-buffer-file-executable-if-script-p)
;; Better defaults:1 ends here

;; [[file:~/.emacs.d/readme.org::*MacOS%20specifics][MacOS specifics:1]]
(use-package exec-path-from-shell
  :if (and (eq system-type 'darwin) (display-graphic-p))
  :config
  (progn
    (when (string-match-p "/zsh$" (getenv "SHELL"))
      (validate-setq exec-path-from-shell-arguments '("-l")))
    (exec-path-from-shell-initialize)))

(when (system-is-mac)
  (setq ns-function-modifier      'hyper ; set fn key to hyper key  (fn = hyper)
        ns-right-command-modifier 'hyper ; set right command key to hyper (control = hyper)
        ns-command-modifier       'meta  ; set command key to meta  (command = meta)
        ns-option-modifier        'super ; set option key to super  (option = super)
        ns-pop-up-frames nil)       ; open file on current buffer when double-clicking it on Mac Finder

  (defun my/swap-meta-and-super ()
    "Swap the mapping of Meta and Super.
Very useful for people using their Mac with a
Windows external keyboard from time to time."
    (interactive)
    (if (eq mac-command-modifier 'super)
        (progn
          (setq mac-command-modifier 'meta)
          (setq mac-option-modifier 'super)
          (message "Command is now bound to META and Option is bound to SUPER."))
      (progn
        (setq mac-command-modifier 'super)
        (setq mac-option-modifier 'meta)
        (message "Command is now bound to SUPER and Option is bound to META."))))
  )
;; MacOS specifics:1 ends here

;; [[file:~/.emacs.d/readme.org::*Location][Location:1]]
(setq calendar-week-start-day  1
      calendar-latitude 43.8
      calendar-longitude 11.0
      calendar-location-name "Prato, Italy")
;; Location:1 ends here

;; [[file:~/.emacs.d/readme.org::*Holidays][Holidays:1]]
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
;; Holidays:1 ends here

;; [[file:~/.emacs.d/readme.org::*History][History:1]]
(setq-default history-length 1000
              history-delete-duplicates t
              savehist-save-minibuffer-history 1
              savehist-additional-variables '(kill-ring
                                              search-ring
                                              regexp-search-ring))
(savehist-mode t)
;; History:1 ends here

;; [[file:~/.emacs.d/readme.org::*Scrolling][Scrolling:1]]
(setq scroll-margin 0
      scroll-conservatively 100000
      scroll-preserve-screen-position 1
      mouse-wheel-scroll-amount '(1 ((shift) . 1))
      mouse-wheel-progressive-speed nil
      mouse-wheel-follow-mouse 't)
;; Scrolling:1 ends here

;; [[file:~/.emacs.d/readme.org::*Useful%20functions][Useful functions:1]]
(put 'downcase-region 'disabled nil)
(put 'upcase-region 'disabled nil)
(put 'narrow-to-region 'disabled nil)
(put 'narrow-to-page 'disabled nil)
;; Useful functions:1 ends here

;; [[file:~/.emacs.d/readme.org::*Split%20window%20and%20move][Split window and move:1]]
  (defun split-below-and-move ()
    (interactive)
    (split-window-below)
    (other-window 1))

  (defun split-right-and-move ()
    (interactive)
    (split-window-right)
    (other-window 1))

  (bind-keys ("C-x 2" . split-below-and-move)
             ("C-x 3" . split-right-and-move))
;; Split window and move:1 ends here

;; [[file:~/.emacs.d/readme.org::*Kill%20the%20current%20buffer][Kill the current buffer:1]]
(defun my/kill-this-buffer ()
  "Kill the current buffer."
  (interactive)
  (kill-buffer (current-buffer)))

(global-set-key (kbd "C-x k") 'my/kill-this-buffer)
;; Kill the current buffer:1 ends here

;; [[file:~/.emacs.d/readme.org::*Improved%20kill-word][Improved kill-word:1]]
(defun my/kill-word ()
  "Kill the entire word your cursor is in. Equivalent to 'ciw' in vim."
  (interactive)
  (forward-char 1)
  (backward-word)
  (kill-word 1))

(global-set-key (kbd "H-d w") 'my/kill-word)
;; Improved kill-word:1 ends here

;; [[file:~/.emacs.d/readme.org::*Improved%20copy-word][Improved copy-word:1]]
(defun my/copy-word ()
  "Copy the whole word your cursor is in."
  (interactive)
  (save-excursion (forward-char 1)
                  (backward-word)
                  (kill-word 1)
                  (yank)))

(global-set-key (kbd "H-c w") 'my/copy-word)
;; Improved copy-word:1 ends here

;; [[file:~/.emacs.d/readme.org::*Copy%20a%20line][Copy a line:1]]
(defun my/copy-line ()
  "Copies a line regardless the cursor position."
  (interactive)
  (save-excursion (kill-new
                   (buffer-substring
                    (point-at-bol)
                    (point-at-eol)))))

(global-set-key (kbd "H-c l") 'my/copy-line)
;; Copy a line:1 ends here

;; [[file:~/.emacs.d/readme.org::*Org%20header%20IDs][Org header IDs:1]]
  (defun my/copy-id-to-clipboard()
    "Copy the ID property value to killring,
  if no ID is there then create a new unique ID.
  This function works only in org-mode buffers.

  The purpose of this function is to easily construct id:-links to
  org-mode items. If its assigned to a key it saves you marking the
  text and copying to the killring."
         (interactive)
         (when (eq major-mode 'org-mode) ; do this only in org-mode buffers
       (setq mytmpid (funcall 'org-id-get-create))
       (kill-new mytmpid)
       (message "Copied %s to killring (clipboard)" mytmpid)
         ))

  (global-set-key (kbd "<f5>") 'my/copy-id-to-clipboard)
;; Org header IDs:1 ends here

;; [[file:~/.emacs.d/readme.org::*Create%20parent%20directories%20when%20saving%20file][Create parent directories when saving file:1]]
(defun sensible-defaults/offer-to-create-parent-directories-on-save ()
  "When saving a file in a directory that doesn't exist, offer
to (recursively) create the file's parent directories."
  (add-hook 'before-save-hook
            (lambda ()
              (when buffer-file-name
                (let ((dir (file-name-directory buffer-file-name)))
                  (when (and (not (file-exists-p dir))
                             (y-or-n-p (format "Directory %s does not exist. Create it?" dir)))
                    (make-directory dir t)))))))
;; Create parent directories when saving file:1 ends here

;; [[file:~/.emacs.d/readme.org::*general.el][general.el:1]]
  (use-package general
    :config
    (general-define-key
     :prefix "H-SPC"
     ;; applications
     "a" '(:ignore t :which-key "Applications")
     "ad" 'dired-jump
     "am" 'notmuch
     "ag" 'magit-status

     ;; buffers
     "b" '(:ignore t :which-key "Buffers")
     "bb" 'switch-to-buffer
     "bd" 'kill-this-buffer
     "by" 'copy-whole-buffer

     ;; ;; files
     "f" '(:ignore t :which-key "Files")
     "ff" 'find-file
     "fl" 'load-file
     "fs" 'save-buffer

  ;; edit text
     "e" '(:ignore t :which-key "Edit")
     "em" 'hydra-mc/body

     ;; ;; spelling
     "s" '(:ignore t :which-key "Spelling")
     "sb" 'flyspell-buffer
     "sd" 'switch-dictionary
     "sn" 'flyspell-goto-next-error
     "sp" 'flyspell-popup-correct
     "sw" 'flyspell-auto-correct-word
     "sz" 'flyspell-lazy-check-buffer

     ;; modes
     "m" '(:ignore t :which-key "Modes")
     "ml" 'global-display-line-numbers-mode
     "mw" 'whitespace-mode)

    (general-define-key
     :keymaps 'dired-mode-map
     :prefix ","
     "&" 'dired-do-async-shell-command
     "s" 'dired-get-size
     "_" 'xah-dired-rename-space-to-underscore
     "-" 'xah-dired-rename-space-to-hyphen))
;; general.el:1 ends here

;; [[file:~/.emacs.d/readme.org::*Package][Package:1]]
(use-package hydra
  :bind (("<f2>" . hydra-zoom/body))
  ;;:chords (("ww" . hydra-window/body)
  ;;         ("jz" . hydra-zoom/body))
  :config
  (setq lv-use-separator t)
  (set-face-attribute 'hydra-face-blue nil
                      :foreground "deep sky blue"
                      :weight 'bold))
;; Package:1 ends here

;; [[file:~/.emacs.d/readme.org::*text-size%20->%20hydra-zoom][text-size -> hydra-zoom:1]]
(defhydra hydra-zoom (:color red
                             :columns nil)
  "zoom"
  ("=" text-scale-increase "in")
  ("-" text-scale-decrease "out")
  ("0" (text-scale-adjust 0) "reset")
  ("q" nil "quit" :color blue))
;; text-size -> hydra-zoom:1 ends here

;; [[file:~/.emacs.d/readme.org::*window%20management%20->%20hydra-window][window management -> hydra-window:1]]
(defhydra hydra-window (:color red
                               :columns nil)
  "window"
  ("h" windmove-left nil)
  ("j" windmove-down nil)
  ("k" windmove-up nil)
  ("l" windmove-right nil)
  ("2" split-below-and-move "vert")
  ("3" split-right-and-move "horz")
  ("t" transpose-frame "'" :exit t)
  ("a" ace-window "ace")
  ("s" ace-swap-window "swap")
  ("d" ace-delete-window "del")
  ("o" ace-maximize-window "ace-one" :exit t)
  ("b" ivy-switch-buffer "buf")
  ("m" headlong-bookmark-jump "bmk")
  ("q" nil "cancel")
  ("z" aw-switch-buffer-in-window "bufwin")
  ("u" (progn
         (winner-undo)
         (setq this-command 'winner-undo)) "undo"))
;; window management -> hydra-window:1 ends here

;; [[file:~/.emacs.d/readme.org::*hydra-git][hydra-git:1]]
;; create a hydra for git and bind it to SPC g
(defhydra hydra-git (:color red
                            :columns nil)
  "A hydra for git!"
  ("g" magit-status "magit" :color blue)
  ("j" git-gutter:next-hunk "next")
  ("k" git-gutter:previous-hunk "previous")
  ("d" git-gutter:popup-hunk "diff")
  ("s" git-gutter:stage-hunk "stage")
  ("r" git-gutter:revert-hunk "revert")
  ("m" git-gutter:mark-hunk "mark")
  ("q" nil "cancel" :color blue))
;; hydra-git:1 ends here

;; [[file:~/.emacs.d/readme.org::*%5B%5Bhttps://github.com/justbur/emacs-which-key%5D%5Bwhich-key%5D%5D][[[https://github.com/justbur/emacs-which-key][which-key]]:1]]
(use-package which-key
  :init
  (which-key-mode)
  :config
  (validate-setq which-key-idle-delay 0.5
                 which-key-compute-remaps t ; show correct descriptions for remapped keys
                 which-key-allow-multiple-replacements t)

  (validate-setq which-key-replacement-alist
                 ;; Replacements for how part or whole of FUNCTION is replaced when which-key
                 ;; displays
                 ;; KEY → FUNCTION
                 '(((nil . "Prefix Command") . (nil . "prefix"))
                   ((nil . "which-key-show-next-page") . (nil . "next pg"))
                   ((nil . "/body\\'") . (nil . "")) ; Remove display the "/body" portion of hydra fn names
                   ((nil . "\\`hydra-") . (nil . "+h/"))
                   ((nil . "\\`org-babel-") . (nil . "ob/"))
                   ((nil . "\\`artist-select-op-") . (nil . "")) ; Make artist-mode function names less verbose
                   ((nil . "\\`artist-select-") . (nil . "sel-"))
                   ((nil . "\\`artist-toggle-") . (nil . "toggle-"))
                   ((nil . "\\`engine/search-") . (nil . "🔎 "))

                   ;; Replacements for how KEY is replaced when which-key displays
                   ;;   KEY → FUNCTION
                   ;; Eg: After "C-c", display "right → winner-redo" as "▶ → winner-redo"
                   (("<left>"    . nil) . ("◀" . nil)) ; ←
                   (("<right>"   . nil) . ("▶" . nil)) ; →
                   (("<up>"      . nil) . ("▲" . nil)) ; ↑
                   (("<down>"    . nil) . ("▼" . nil)) ; ↓
                   (("\\`DEL\\'" . nil) . ("⌫" . nil)) ; backspace key
                   (("<next>"    . nil) . ("PgDn" . nil))
                   (("<prior>"   . nil) . ("PgUp" . nil))
                   (("RET"       . nil) . ("⏎" . nil))
                   (("deletechar" . nil) . ("⌦" . nil))))

  ;; Change what string to display for a given *complete* key binding
  ;; Eg: After "C-x", display "8 → +unicode" instead of "8 → +prefix"
  (which-key-add-key-based-replacements
    "C-x 8"   "unicode"
    "C-x a"   "abbrev/expand"
    "C-x r"   "rectangle/register/bookmark"
    "C-x v"   "VC"
    "C-c /" "engine-mode-map"
    "C-c C-v" "org-babel"))
;; [[https://github.com/justbur/emacs-which-key][which-key]]:1 ends here

;; [[file:~/.emacs.d/readme.org::*Defaults][Defaults:1]]
(use-package org
  :general  (("C-c a" 'org-agenda-list)
             ("C-c c" 'org-capture)
             ("C-c l" 'org-store-link)
             ;;("C-c f" . org-cycle-agenda-files)
             ;;("C-c s" . org-search-view)
             ("C-c t" 'org-todo-list)
             ("s-j"   'org-babel-next-src-block)
             ("s-k"   'org-babel-previous-src-block)
             ("s-l"   'org-edit-src-code)
             (org-src-mode-map
             "s-l" 'org-edit-src-exit)
             (org-mode-map
              "c" 'org-capture
              "d" 'org-clock-display
              "i" 'org-clock-in
              "o" 'org-clock-out))
  :config
  (validate-setq
   ;;org-tags-column 90                  ; column to which the tags have to be indented
   org-ellipsis "  "                   ; ⬎, ⤷, ⤵, ⚡,  
   org-fontify-whole-heading-line t     ; fontify the whole line for headings
   org-fontify-done-headline t
   org-fontify-quote-and-verse-blocks t
   org-startup-indented t
   org-cycle-include-plain-lists t
   org-list-allow-alphabetical t
   org-preview-latex-default-process 'imagemagick ; preview latex fragments

   ;; Code blocks to play nicelly on org-babel
   org-edit-src-content-indentation 0 ; number of whitespaces added to the code block indentation
   org-src-tab-acts-natively t    ; TAB acts natively as it was in the language major mode
   org-src-preserve-indentation t ; preserve indentation when exporting blocks
   org-src-fontify-natively t     ; highlights code-blocks natively
   org-src-window-setup 'current-window ; open code-blocks in the current window
   org-confirm-babel-evaluate nil  ; don't ask for confirmation when compiling code-blocks

   ;; Files location
   ;;org-directory "~/org"
   org-default-notes-file "notes.org"
   org-agenda-files (list "~/org/todo.org"
                          "~/org/clockin.org"))

  ;; Refile
  ;; https://blog.aaronbieber.com/2017/03/19/organizing-notes-with-refile.html
  (setq org-refile-targets '(("www.org" :maxlevel . 2)))
  (setq org-refile-use-outline-path t)          ; Show full paths for refiling
  (setq org-outline-path-complete-in-steps nil) ; Refile in a single go
  (setq org-refile-allow-creating-parent-nodes 'confirm)

  ;; enter “<el” and hit tab creates a template for elisp insertion
  (add-to-list 'org-structure-template-alist
               '("el" . "src emacs-lisp"))

  ;; TODO workflow states
  org-todo-keywords
  '("☛ TODO(t)" "⚑ WAIT(w@)" "|" "✔ DONE(d)" "✘ CANCEL(c@)")

  ;; TODO fontification
  org-todo-keyword-faces
  '(("☛ TODO"   . (:foreground "#ff4500" :weight bold))
    ("✔ DONE"   . (:foreground "#00ff7f" :weight bold))
    ("⚑ WAIT"   . (:foreground "#ffff00" :weight bold))
    ("✘ CANCEL" . (:foreground "#00bfff" :weight bold)))
  )
;; Defaults:1 ends here

;; [[file:~/.emacs.d/readme.org::*org-babel][org-babel:1]]
(use-package ob
  :ensure nil
  :config
  (org-babel-do-load-languages
   (quote org-babel-load-languages)
   (quote ((calc . t)
           (clojure . t)
           (ditaa . t)
           (dot . t)
           (emacs-lisp . t)
           (gnuplot . t)
           (latex . t)
           (ledger . t)
           (octave . t)
           (org . t)
           (makefile . t)
           (plantuml . t)
           (python . t)
           (R . t)
           (ruby . t)
           (shell . t)
           (sqlite . t)
           (sql . t)
           ))))
;; org-babel:1 ends here

;; [[file:~/.emacs.d/readme.org::*org-clock][org-clock:1]]
(use-package org-clock
  :ensure nil
  :config
  (org-clock-persistence-insinuate)           ; resume clocking task when emacs is restarted
  (validate-setq
   org-clock-persist t                        ; save all clock history when exiting Emacs, load it on startup
   org-clock-persist-query-resume nil         ; do not prompt to resume an active clock
   org-clock-history-length 10                ; show lot of clocking history from where choose items
   org-clock-in-resume t                      ; resume clocking task on clock-in if the clock is open
   org-clock-clocked-in-display nil           ; don't show current task in the modeline
   org-clock-into-drawer "CLOCKING"           ; clocking goes into specfic drawer
   org-clock-report-include-clocking-task t)) ; include current clocking task in clock reports
;; org-clock:1 ends here

;; [[file:~/.emacs.d/readme.org::*org-capture-templates][org-capture-templates:1]]
(use-package org-capture
  :ensure nil
  :config
  (setq org-capture-templates
        '(("p" "Protocol Quote" entry
           (file+headline "www.org" "Bookmarks")
           "* %?[[%:link][%:description]]\n:PROPERTIES:\n:CREATED: %U\n:END:\n\n#+BEGIN_QUOTE\n%i\n#+END_QUOTE\n"
           :empty-lines 1
           :immediate-finish
           )

          ("L" "Protocol Link" entry
           (file+headline "www.org" "Bookmarks")
           "* %?[[%:link][%:description]]\n:PROPERTIES:\n:CREATED: %U\n:END:\n"
           :empty-lines 1
           :immediate-finish
           )

          ("t" "Tasks" entry
           (file+headline "tasks.org" "Tasks")
           "* ☛ TODO %^{Task} %^g\n:PROPERTIES:\n:CREATED: %U\n:END:\n%?%i"
           :empty-lines 1)

          ("n" "Notes" entry
           (file+headline "notes.org" "Notes")
           "* %^{Header} %^G\n %u\n %?")

          ("j" "Journal" entry
           (file+datetree "journal.org")
           "* %U %^{Title}\n %?%i\n %a")

          ("a" "Articles" entry
           (file+headline "articles.org" "Articles")
           "* %^{Title} %^g\n:PROPERTIES:\n:CREATED: %U\n:END:\n%?%i\n"
           :empty-lines 1
           :immediate-finish)
          )))
;; org-capture-templates:1 ends here

;; [[file:~/.emacs.d/readme.org::*org-bullets][org-bullets:1]]
(use-package org-bullets
  :hook (org-mode . (lambda () (org-bullets-mode t)))
  :config
  ;;(setq org-bullets-bullet-list '("☯" "☰" "☱" "☲" "☳" "☴" "☵" "☶" "☷"))
  ;;(setq org-bullets-bullet-list '("♣" "♥" "♠" "♦" "♧" "♡" "♤" "♢"))
  ;;(validate-setq org-bullets-bullet-list '("☯" "☉" "∞" "◉" "⊚" "☀" "☾" "☥"))
  (validate-setq org-bullets-bullet-list '("◉" "☉" "⊚" "○" "∞"))

  ;; make available "org-bullet-face" such that I can control the font size individually
  (validate-setq org-bullets-face-name (quote org-bullet-face))
  ;; (custom-set-faces '(org-bullet-face
  ;;                     ((t (:foreground "burlywood"
  ;;                                      :weight normal
  ;;                                      :height 1.6)))))
  )
;; org-bullets:1 ends here

;; [[file:~/.emacs.d/readme.org::*ox.el][ox.el:1]]
(use-package ox
  :ensure nil
  :config
  (validate-setq org-export-with-smart-quotes t
                 org-export-allow-bind-keywords t
                 org-latex-listings 'minted
                 org-latex-packages-alist '(("" "color" t)
                                            ("" "minted" t)
                                            ("" "parskip" t)
                                            ("" "tikz" t)))

  (when (system-is-mac)
    (validate-setq org-ditaa-jar-path "/usr/local/bin/ditaa"))

  (setq org-latex-pdf-process
        '("pdflatex -shell-escape -interaction nonstopmode -output-directory %o %f"
          "pdflatex -shell-escape -interaction nonstopmode -output-directory %o %f"
          "pdflatex -shell-escape -interaction nonstopmode -output-directory %o %f"))
  )
;; ox.el:1 ends here

;; [[file:~/.emacs.d/readme.org::*ox-pandoc][ox-pandoc:1]]
(use-package ox-pandoc
  ;;  :after org-plus-contrib
  :config
  (validate-setq org-pandoc-options '((standalone . t)
                                      (mathjax . t)
                                      (highlight-style . "pygments"))
                 org-pandoc-options-for-docx '((standalone . nil))
                 org-pandoc-options-for-beamer-pdf '((pdf-engine . "lualatex"))
                 org-pandoc-options-for-latex-pdf  '((pdf-engine . "lualatex"))
                 ;;org-pandoc-options-for-latex-pdf '((latex-engine . "xelatex")
                 ;;                                   (template . "~/.pandoc/templates/memoir2.latex" ))

                 ;; Use external css for html5
                 ;; (let ((stylesheet (expand-file-name
                 ;;                    (locate-user-emacs-file "etc/pandoc.css"))))
                 ;;   (setq org-pandoc-options-for-html5
                 ;;         `((css . ,(concat "file://" stylesheet)))))
                 ))
;; ox-pandoc:1 ends here

;; [[file:~/.emacs.d/readme.org::*%5B%5Bhttps://github.com/snosov1/toc-org%5D%5Btoc-org%5D%5D][[[https://github.com/snosov1/toc-org][toc-org]]:1]]
(use-package toc-org
  :hook (org-mode . toc-org-enable))
;; [[https://github.com/snosov1/toc-org][toc-org]]:1 ends here

;; [[file:~/.emacs.d/readme.org::*org-brain][org-brain:1]]
(use-package org-brain
  :config
  (push '("b" "Brain" plain (function org-brain-goto-end)
          "* %i%?" :empty-lines 1)
        org-capture-templates)
  (setq org-brain-visualize-default-choices 'all)
  (setq org-brain-title-max-length 12))
;; org-brain:1 ends here

;; [[file:~/.emacs.d/readme.org::*ivy][ivy:1]]
(use-package ivy
  :bind (("C-c C-r"  . ivy-resume)
         :map ivy-minibuffer-map
         ("<return>" . ivy-alt-done)
         ("C-M-h"    . ivy-previous-line-and-call)
         ("C-:"      . ivy-dired)
         ("C-c o"    . ivy-occur)
         ("C-j"      . ivy-next-line)
         ("C-k"      . ivy-previous-line)
         ("C-l"      . ivy-alt-done)
         :map read-expression-map
         ("C-r"      . counsel-expression-history))
  :chords (("bb" . ivy-switch-buffer))
  :config
  (ivy-mode 1)
  (validate-setq ivy-use-virtual-buffers t         ; list `recentf' and bookmarks as well
                 ivy-height 15
                 ivy-count-format "(%d/%d) "       ; counter
                 ivy-extra-directories nil         ; Do not show "./" and "../"
                 ivy-virtual-abbreviate 'full      ; Show full file path
                 ivy-re-builders-alist '((t . ivy--regex-ignore-order))
                 ivy-use-ignore-default 'always    ; ignore buffers set in `ivy-ignore-buffers'
                 ivy-ignore-buffers                ; ignore some buffers in `ivy-switch-buffer'
                 '("company-statistics-cache.el" ".elfeed/index"))

  (defun ivy-dired ()
    (interactive)
    (if ivy--directory
        (ivy-quit-and-run
         (dired ivy--directory)
         (when (re-search-forward
                (regexp-quote
                 (substring ivy--current 0 -1)) nil t)
           (goto-char (match-beginning 0))))
      (user-error
       "Not completing files currently")))

  ;; Customize faces per mode
  ;;(validate-setq ivy-switch-buffer-faces-alist
  ;;               '((emacs-lisp-mode . swiper-match-face-1)
  ;;                 (dired-mode . ivy-subdir)
  ;;                 (org-mode . org-level-4)))
  )

;; Speed up my workflow with prearranged windows
;; (setq ivy-views '(("boccaperta + ba-server [–]"
;;                    (vert
;;                     (sexp (bookmark-jump "boccaperta"))
;;                     (sexp (bookmark-jump "ba-server"))))
;;                   ("desktop + ba-server [–]"
;;                    (vert
;;                     (sexp (bookmark-jump "desktop"))
;;                     (sexp (bookmark-jump "ba-server"))))))

;; Hydra bindings for ivy buffer
(use-package ivy-hydra
  :after ivy)

(use-package all-the-icons-ivy
  :after all-the-icons
  :config
  (all-the-icons-ivy-setup))

;; smex order selections accordingly to the most used ones
(use-package smex)
;; ivy:1 ends here

;; [[file:~/.emacs.d/readme.org::*counsel][counsel:1]]
(use-package counsel
  :bind (("C-h l"   . counsel-load-library)
         ;;("<f2> u"  . counsel-unicode-char)
         ("C-r"     . counsel-grep-or-swiper)
         ("C-c g"   . counsel-git)
         ("C-c j"   . counsel-git-grep)
         ("C-c k"   . counsel-rg)
         ("C-x l"   . counsel-locate)
         ("C-c r"   . counsel-linux-app)
         ("C-x i"   . counsel-imenu))
  :config
  (counsel-mode 1)
  (validate-setq  counsel-mode-override-describe-bindings t
                  counsel-find-file-at-point t
                  counsel-find-file-ignore-regexp
                  (concat
                   "\\(?:\\`[#.]\\)"              ; file names beginning with # or .
                   "\\|\\(?:\\`.+?[#~]\\'\\)"     ; file names ending with # or ~
                   )))

(use-package counsel-projectile
  :after projectile
  :config
  (counsel-projectile-mode))
;; counsel:1 ends here

;; [[file:~/.emacs.d/readme.org::*swiper][swiper:1]]
(use-package swiper
  :bind (("C-s" . swiper))
  :config
  ;; always recenter when leaving swiper
  (validate-setq swiper-action-recenter t))
;; swiper:1 ends here

;; [[file:~/.emacs.d/readme.org::*projectile][projectile:1]]
(use-package projectile
  :config
  (setq projectile-enable-caching t
        projectile-completion-system 'ivy
        projectile-switch-project-action 'projectile-dired
        projectile-mode-line '(:eval (format
                                      " :%s:" (projectile-project-name))))
  (projectile-mode))

(use-package ibuffer-projectile)
;; projectile:1 ends here

;; [[file:~/.emacs.d/readme.org::*alert][alert:1]]
(use-package alert)
;; alert:1 ends here

;; [[file:~/.emacs.d/readme.org::*anzu][anzu:1]]
(use-package anzu
  :bind (("M-%" . anzu-query-replace)
         ("C-M-%" . anzu-query-replace-regexp))
  :config
  (validate-setq anzu-cons-mode-line-p nil
                 anzu-replace-to-string-separator "  ")
  (global-anzu-mode +1))
;; anzu:1 ends here

;; [[file:~/.emacs.d/readme.org::*async][async:1]]
(use-package async
  :config
  (async-bytecomp-package-mode t))
;; async:1 ends here

;; [[file:~/.emacs.d/readme.org::*bookmarks][bookmarks:1]]
(use-package bookmark
  :config
  (setq bookmark-completion-ignore-case nil)
  (bookmark-maybe-load-default-file))
;; bookmarks:1 ends here

;; [[file:~/.emacs.d/readme.org::*easy-kill][easy-kill:1]]
(use-package easy-kill
  :bind (([remap kill-ring-save] . easy-kill)
         ([remap mark-sexp] . easy-mark)))
;; easy-kill:1 ends here

;; [[file:~/.emacs.d/readme.org::*ediff][ediff:1]]
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
;; ediff:1 ends here

;; [[file:~/.emacs.d/readme.org::*esup][esup:1]]
(use-package esup
  :config
  (setq esup-insignificant-time 0.001))
;; esup:1 ends here

;; [[file:~/.emacs.d/readme.org::*fill-column-indicator][fill-column-indicator:1]]
(use-package fill-column-indicator
  :disabled t
  :config
  (validate-setq fci-rule-width 1
                 fci-rule-color "#5d478b"
                 fci-rule-column 80)
  (define-globalized-minor-mode global-fci-mode fci-mode
    (lambda ()
      (fci-mode 1)))
  (global-fci-mode 1))
;; fill-column-indicator:1 ends here

;; [[file:~/.emacs.d/readme.org::*fixmee][fixmee:1]]
(use-package wiki-nav
  :config
  (global-wiki-nav-mode 1))

(use-package fixmee
  :after wiki-nav
  :config
  (global-fixmee-mode 1))
;; fixmee:1 ends here

;; [[file:~/.emacs.d/readme.org::*flx][flx:1]]
(use-package flx)
;; flx:1 ends here

;; [[file:~/.emacs.d/readme.org::*flycheck][flycheck:1]]
(use-package flycheck
  :init (global-flycheck-mode)
  :config
  (setq flycheck-mode-line
        '(:eval
          (pcase flycheck-last-status-change
            (`not-checked nil)
            (`no-checker (propertize " -" 'face 'warning))
            (`running (propertize " ✷" 'face 'success))
            (`errored (propertize " ☠" 'face 'error))
            (`finished
             (let* ((error-counts (flycheck-count-errors flycheck-current-errors))
                    (no-errors (cdr (assq 'error error-counts)))
                    (no-warnings (cdr (assq 'warning error-counts)))
                    (face (cond (no-errors 'error)
                                (no-warnings 'warning)
                                (t 'success))))
               (propertize (format " %s/%s" (or no-errors 0) (or no-warnings 0))
                           'face face)))
            (`interrupted " -")
            (`suspicious '(propertize " ?" 'face 'warning)))))
  )
;; flycheck:1 ends here

;; [[file:~/.emacs.d/readme.org::*Linting%20prose][Linting prose:1]]
(flycheck-define-checker proselint
  "A linter for prose."
  :command ("proselint" source-inplace)
  :error-patterns
  ((warning line-start (file-name) ":" line ":" column ": "
            (id (one-or-more (not (any " "))))
            (message (one-or-more not-newline)
                     (zero-or-more "\n" (any " ") (one-or-more not-newline)))
            line-end))
  :modes (text-mode markdown-mode gfm-mode org-mode))

(add-to-list 'flycheck-checkers 'proselint)
;; Linting prose:1 ends here

;; [[file:~/.emacs.d/readme.org::*graphviz-dot-mode][graphviz-dot-mode:1]]
(use-package graphviz-dot-mode)
;; graphviz-dot-mode:1 ends here

;; [[file:~/.emacs.d/readme.org::*page-break-lines][page-break-lines:1]]
(use-package page-break-lines
  :init (global-page-break-lines-mode))
;; page-break-lines:1 ends here

;; [[file:~/.emacs.d/readme.org::*paradox][paradox:1]]
(use-package paradox
  ;;:disabled t
  :config
  (setq-default paradox-column-width-package 27
                paradox-column-width-version 13
                paradox-execute-asynchronously t
                paradox-github-token t
                paradox-hide-wiki-packages t)
  (set-face-attribute 'paradox-homepage-button-face nil :italic nil)
  (remove-hook 'paradox--report-buffer-print 'paradox-after-execute-functions))
;; paradox:1 ends here

;; [[file:~/.emacs.d/readme.org::*pass][pass:1]]
(use-package pass)
;; pass:1 ends here

;; [[file:~/.emacs.d/readme.org::*pcache][pcache:1]]
(use-package pcache)

(use-package persistent-soft
  :after pcache)
;; pcache:1 ends here

;; [[file:~/.emacs.d/readme.org::*recentf][recentf:1]]
(use-package recentf
  :config
  (recentf-mode t)
  (validate-setq recentf-max-saved-items 10)

  (add-to-list 'recentf-exclude no-littering-var-directory)
  (add-to-list 'recentf-exclude no-littering-etc-directory)
  (add-to-list 'recentf-exclude '("COMMIT_MSG"
                                  "COMMIT_EDITMSG"
                                  "github.*txt$"
                                  ".*png$")))
;; recentf:1 ends here

;; [[file:~/.emacs.d/readme.org::*restart-emacs][restart-emacs:1]]
(use-package restart-emacs
 ;; :general (d/leader-keys "qr" 'restart-emacs)
)
;; restart-emacs:1 ends here

;; [[file:~/.emacs.d/readme.org::*ripgrep][ripgrep:1]]
(use-package rg)
;; ripgrep:1 ends here

;; [[file:~/.emacs.d/readme.org::*uniquify][uniquify:1]]
(use-package uniquify
  :ensure nil
  :config
  (setq uniquify-buffer-name-style 'reverse
        uniquify-separator " • "
        uniquify-after-kill-buffer-p t       ; rename after killing uniquified
        uniquify-ignore-buffers-re "^\\*"))  ; don't muck with special buffers
;; uniquify:1 ends here

;; [[file:~/.emacs.d/readme.org::*undo-tree][undo-tree:1]]
(use-package undo-tree
  :chords (("uu" . undo-tree-visualize))
  :init
  (global-undo-tree-mode))
;; undo-tree:1 ends here

;; [[file:~/.emacs.d/readme.org::*volatile-highlights][volatile-highlights:1]]
(use-package volatile-highlights
  :config
  (volatile-highlights-mode t))
;; volatile-highlights:1 ends here

;; [[file:~/.emacs.d/readme.org::*wgrep][wgrep:1]]
(use-package wgrep
  :config
  (progn
    (with-eval-after-load 'grep
      (bind-key "C-x C-q" #'wgrep-change-to-wgrep-mode grep-mode-map))

    (with-eval-after-load 'wgrep
      (bind-key "C-c C-c" #'wgrep-finish-edit grep-mode-map))))
;; wgrep:1 ends here

;; [[file:~/.emacs.d/readme.org::*eyebrowse][eyebrowse:1]]
(use-package eyebrowse
;; :general (d/leader-keys
;;            "e"  '(:ignore t :wk "eyebrowse")
;;            "es" 'eyebrowse-switch-to-window-config
;;            "el" 'eyebrowse-next-window-config
;;            "eh" 'eyebrowse-prev-window-config
;;            "er" 'eyebrowse-rename-window-config
;;            "ec" 'eyebrowse-close-window-config
;;            "e'" 'eyebrowse-last-window-config
;;            "e0" 'eyebrowse-switch-to-window-config-0 :wk "workspace 0"
;;            "e1" 'eyebrowse-switch-to-window-config-1
;;            "e2" 'eyebrowse-switch-to-window-config-2
;;            "e3" 'eyebrowse-switch-to-window-config-3
;;            "e4" 'eyebrowse-switch-to-window-config-4
;;            "e5" 'eyebrowse-switch-to-window-config-5
;;            "e6" 'eyebrowse-switch-to-window-config-6
;;            "e7" 'eyebrowse-switch-to-window-config-7
;;            "e8" 'eyebrowse-switch-to-window-config-8
;;            "e9" 'eyebrowse-switch-to-window-config-9)
  :config
  (setq eyebrowse-wrap-around t
        eyebrowse-new-workspace t
        eyebrowse-switch-back-and-forth t)

  (eyebrowse-mode t))
;; eyebrowse:1 ends here

;; [[file:~/.emacs.d/readme.org::*%5B%5Bhttps://github.com/nex3/perspective-el%5D%5Bperspective%5D%5D][[[https://github.com/nex3/perspective-el][perspective]]:1]]
(use-package perspective
  :config
  (persp-mode))

(use-package persp-projectile
  :config
  (define-key projectile-mode-map (kbd "s-s")
    'projectile-persp-switch-project))
;; [[https://github.com/nex3/perspective-el][perspective]]:1 ends here

;; [[file:~/.emacs.d/readme.org::*shackle][shackle:1]]
(use-package shackle
  :config
  (validate-setq
   shackle-default-alignment 'below
   shackle-default-size 8
   shackle-rules '(("^\\*ftp " :noselect t :autokill t :noesc t)
                   ;; built-in (emacs)
                   ("*ert*" :same t :modeline t)
                   ("*info*" :size 0.5 :select t :autokill t)
                   ("*Backtrace*" :size 20 :noselect t)
                   ("*Warnings*"  :size 8  :noselect t)
                   ("*Messages*"  :size 12 :noselect t)
                   ("*Help*" :size 0.3)
                   ("^\\*.*Shell Command.*\\*$" :regexp t :size 20 :noselect t :autokill t)
                   (apropos-mode :size 0.3 :autokill t :autoclose t)
                   (Buffer-menu-mode :size 20 :autokill t)
                   (comint-mode :noesc t)
                   (grep-mode :size 25 :noselect t :autokill t)
                   (profiler-report-mode :size 0.3 :regexp t :autokill t :modeline minimal)
                   (tabulated-list-mode :noesc t)
                   (special-mode :noselect t :autokill t :autoclose t)
                   ("^\\*"  :regexp t :noselect t :autokill t)
                   ("^ \\*" :regexp t :size 12 :noselect t :autokill t :autoclose t)))

  (shackle-mode))
;; shackle:1 ends here

;; [[file:~/.emacs.d/readme.org::*ace-link][ace-link:1]]
(use-package ace-link
  :config (ace-link-setup-default))
;; ace-link:1 ends here

;; [[file:~/.emacs.d/readme.org::*ace-window][ace-window:1]]
(use-package ace-window
  :bind ([remap other-window] . ace-window)
  :init
  (setq aw-keys '(?a ?s ?d ?f ?g ?h ?j ?k ?l))
  ;;(setq aw-background nil)
  (setq aw-dispatch-always t)

  ;; customize face of hint character
  (custom-set-faces '(aw-leading-char-face
                      ((t (:inherit ace-jump-face-foreground
                                    :height 3.0))))))
;; ace-window:1 ends here

;; [[file:~/.emacs.d/readme.org::*avy][avy:1]]
(use-package avy
  :chords (("jj" . avy-goto-word-or-subword-1))
  :config
  (setq avy-background t
        avy-all-windows t
        avy-style 'at-full
        avy-case-fold-search nil)

  (set-face-attribute 'avy-lead-face nil
                      :foreground "gold"
                      :weight 'bold
                      :background nil)
  (set-face-attribute 'avy-lead-face-0 nil
                      :foreground "deep sky blue"
                      :weight 'bold
                      :background nil))
;; avy:1 ends here

;; [[file:~/.emacs.d/readme.org::*%5B%5Bhttps://magit.vc%5D%5Bmagit%5D%5D][[[https://magit.vc][magit]]:1]]
(use-package magit
  :hook (after-save . magit-after-save-refresh-status) ; refreshes magit status when file is saved
  :config
  (setq magit-completing-read-function 'ivy-completing-read
        magit-display-buffer-function 'magit-display-buffer-fullframe-status-topleft-v1)

  ;; automatically refreshes magit status after file is saved
  ;;(add-hook 'after-save-hook 'magit-after-save-refresh-status)
  )
;; [[https://magit.vc][magit]]:1 ends here

;; [[file:~/.emacs.d/readme.org::*git-gutter][git-gutter:1]]
(use-package git-gutter
  :init
  (global-git-gutter-mode +1))
;; git-gutter:1 ends here

;; [[file:~/.emacs.d/readme.org::*git-timemachine][git-timemachine:1]]
(use-package git-timemachine
  :commands git-timemachine)
;; git-timemachine:1 ends here

;; [[file:~/.emacs.d/readme.org::*%5B%5Bhttps://github.com/Malabarba/aggressive-indent-mode%5D%5Baggressive-indent%5D%5D][[[https://github.com/Malabarba/aggressive-indent-mode][aggressive-indent]]:1]]
(use-package aggressive-indent
  :config
  (global-aggressive-indent-mode 1)
  (add-to-list 'aggressive-indent-excluded-modes 'html-mode))
;; [[https://github.com/Malabarba/aggressive-indent-mode][aggressive-indent]]:1 ends here

;; [[file:~/.emacs.d/readme.org::*%5B%5Bhttps://github.com/DarthFennec/highlight-indent-guides%5D%5Bhighligh-indent-guides%5D%5D][[[https://github.com/DarthFennec/highlight-indent-guides][highligh-indent-guides]]:1]]
(use-package highlight-indent-guides
  :disabled t
  :hook (prog-mode . highlight-indent-guides-mode)
  :config
  (validate-setq highlight-indent-guides-method 'character))
;; [[https://github.com/DarthFennec/highlight-indent-guides][highligh-indent-guides]]:1 ends here

;; [[file:~/.emacs.d/readme.org::*%5B%5Bhttps://github.com/Fuco1/smartparens%5D%5Bsmartparens%5D%5D][[[https://github.com/Fuco1/smartparens][smartparens]]:1]]
(use-package lispy
  :hook (prog-mode . lispy-mode))
;; [[https://github.com/Fuco1/smartparens][smartparens]]:1 ends here

;; [[file:~/.emacs.d/readme.org::*%5B%5Bhttps://github.com/Fanael/rainbow-delimiters%5D%5Brainbow-delimiters%5D%5D][[[https://github.com/Fanael/rainbow-delimiters][rainbow-delimiters]]:1]]
(use-package rainbow-delimiters
  :hook (prog-mode . rainbow-delimiters-mode))
;; [[https://github.com/Fanael/rainbow-delimiters][rainbow-delimiters]]:1 ends here

;; [[file:~/.emacs.d/readme.org::*company][company:1]]
;; (use-package company
;;   :hook (after-init . global-company-mode)
;;   :bind (("C-c /" . company-files))                    ; force complete file names on "C-c /" key
;;   :config
;;   (setq company-tooltip-limit 20                       ; bigger popup window
;;         company-tooltip-align-annotations 't           ; align annotations to the right tooltip border
;;         company-idle-delay .3                          ; decrease delay before autocompletion popup shows
;;         company-begin-commands '(self-insert-command)) ; start autocompletion only after typing
;;   )

(use-package company
  :preface (message "[INIT] Package: Company")
  :commands company-mode
  :hook (prog-mode . company-mode))

(use-package company-box
  :disabled t
  :hook (company-mode . company-box-mode)
  :config
  (setq company-box-icons-unknown 'fa_question_circle)

  (setq company-box-icons-elisp
        '((fa_tag :face font-lock-function-name-face) ;; Function
          (fa_cog :face font-lock-variable-name-face) ;; Variable
          (fa_cube :face font-lock-constant-face) ;; Feature
          (md_color_lens :face font-lock-doc-face))) ;; Face

  (setq company-box-icons-yasnippet 'fa_bookmark)

  (setq company-box-icons-lsp
        '((1 . fa_text_height) ;; Text
          (2 . (fa_tags :face font-lock-function-name-face)) ;; Method
          (3 . (fa_tag :face font-lock-function-name-face)) ;; Function
          (4 . (fa_tag :face font-lock-function-name-face)) ;; Constructor
          (5 . (fa_cog :foreground "#FF9800")) ;; Field
          (6 . (fa_cog :foreground "#FF9800")) ;; Variable
          (7 . (fa_cube :foreground "#7C4DFF")) ;; Class
          (8 . (fa_cube :foreground "#7C4DFF")) ;; Interface
          (9 . (fa_cube :foreground "#7C4DFF")) ;; Module
          (10 . (fa_cog :foreground "#FF9800")) ;; Property
          (11 . md_settings_system_daydream) ;; Unit
          (12 . (fa_cog :foreground "#FF9800")) ;; Value
          (13 . (md_storage :face font-lock-type-face)) ;; Enum
          (14 . (md_closed_caption :foreground "#009688")) ;; Keyword
          (15 . md_closed_caption) ;; Snippet
          (16 . (md_color_lens :face font-lock-doc-face)) ;; Color
          (17 . fa_file_text_o) ;; File
          (18 . md_refresh) ;; Reference
          (19 . fa_folder_open) ;; Folder
          (20 . (md_closed_caption :foreground "#009688")) ;; EnumMember
          (21 . (fa_square :face font-lock-constant-face)) ;; Constant
          (22 . (fa_cube :face font-lock-type-face)) ;; Struct
          (23 . fa_calendar) ;; Event
          (24 . fa_square_o) ;; Operator
          (25 . fa_arrows)) ;; TypeParameter
        )
  )

(use-package company-statistics
  :after company
  :hook (after-init . company-statistics-mode))

(use-package slime-company
  :after company
  :config
  (slime-setup '(slime-fancy slime-company)))

(use-package company-ansible
  :after company
  :config
  (add-to-list 'company-backends 'company-ansible))
;; company:1 ends here

;; [[file:~/.emacs.d/readme.org::*language%20server%20protocol][language server protocol:1]]
(use-package lsp-mode
  ;; :hook
  ;; (prog-mode . lsp-mode)
  ;;(lsp-after-open . lsp-enable-imenu)
  :commands (lsp-mode
             lsp-define-stdio-client
             lsp-client-on-notification
             lsp-make-traverser))

(use-package lsp-ui
  :preface  (message "[INIT] Package: LSP UI")
  :after    lsp-mode
  :hook     ((lsp-mode  . lsp-ui-mode)
             (prog-mode . flycheck-mode))
  :commands (lsp-ui-mode
             lsp-ui-peek-find-definistions
             lsp-ui-peek-find-references)
  :config   (setq lsp-ui-doc-enable  t
		  lsp-ui-flychekc-enable t
		  lsp-ui-imenu-enale     nil
		  lsp-ui-peek-enable     t
		  lsp-ui-sideline-enable nil))

(use-package company-lsp
  :preface (message "[INIT] Package: Company LSP")
  :after (lsp-mode company)
  :config
  (push 'company-lsp company-backends))
;; language server protocol:1 ends here

;; [[file:~/.emacs.d/readme.org::*yasnippet][yasnippet:1]]
(use-package yasnippet-snippets)

(use-package yasnippet
  :config
  (yas-global-mode 1))
;; yasnippet:1 ends here

;; [[file:~/.emacs.d/readme.org::*yankpad][yankpad:1]]
(use-package yankpad
  :config
  (validate-setq yankpad-file "~/org/yankpad.org"))
;; yankpad:1 ends here

;; [[file:~/.emacs.d/readme.org::*ascii-doc][ascii-doc:1]]
(use-package adoc-mode
  :config
  (autoload 'adoc-mode "adoc-mode" nil t))
;; ascii-doc:1 ends here

;; [[file:~/.emacs.d/readme.org::*html][html:1]]
(use-package simple-httpd
  :disabled t
  :config
  ;;(setq httpd-root "/var/www")
  (httpd-start))

(use-package htmlize)
;; html:1 ends here

;; [[file:~/.emacs.d/readme.org::*jinja2][jinja2:1]]
(use-package jinja2-mode
  :mode "\\.j2\\'")
;; jinja2:1 ends here

;; [[file:~/.emacs.d/readme.org::*json-mode][json-mode:1]]
(use-package json-mode
  :commands json-mode
  :config
  (bind-keys :map json-mode-map
             ("C-c <tab>" . json-mode-beautify)))
;; json-mode:1 ends here

;; [[file:~/.emacs.d/readme.org::*markdown][markdown:1]]
(use-package markdown-mode)

(use-package impatient-mode
  :disabled t
  :config
  (defun markdown-html (buffer)
    (princ (with-current-buffer buffer
             (format "<!DOCTYPE html><html><title>Impatient Markdown</title><xmp theme=\"united\" style=\"display:none;\"> %s  </xmp><script src=\"http://strapdownjs.com/v/0.2/strapdown.js\"></script></html>"
                     (buffer-substring-no-properties (point-min) (point-max))))
           (current-buffer)))

  (defun markdown-filter (buffer)
    (princ
     (with-temp-buffer
       (let ((tmpname (buffer-name)))
         (set-buffer buffer)
         (set-buffer (markdown tmpname)) ; the function markdown is in `markdown-mode.el'
         (buffer-string)))
     (current-buffer)))
  )

(use-package markdown-toc)
;; markdown:1 ends here

;; [[file:~/.emacs.d/readme.org::*yaml][yaml:1]]
(use-package yaml-mode)
;; yaml:1 ends here

;; [[file:~/.emacs.d/readme.org::*python][python:1]]
;; (use-package lsp-python
;;   :hook (python-mode . lsp-python-enable))

(setq python-indent 2)

(use-package anaconda-mode
  :hook ((python-mode . anaconda-mode)
         (python-mode . anaconda-eldoc-mode))
  :init
  (add-to-list 'auto-mode-alist '("\\.py$" . python-mode))
)

;; (use-package python
;;   :mode ("\\.py\\'" . python-mode)
;;   :hook ((python-mode . lsp-mode)
;; 	 (python-mode . lsp-python-enable)))

(use-package pyenv-mode
  :init
  (add-to-list 'exec-path "~/.pyenv/shims")
  (setenv "WORKON_HOME" "~/.pyenv/versions/")
  :config
  (pyenv-mode)
  :bind
  ("C-x p e" . pyenv-activate-current-project))


(defun projectile-pyenv-mode-set ()
  "Set pyenv version matching project name."
  (let ((project (projectile-project-name)))
    (if (member project (pyenv-mode-versions))
        (pyenv-mode-set project)
      (pyenv-mode-unset))))

(add-hook 'projectile-after-switch-project-hook 'projectile-pyenv-mode-set)
;; python:1 ends here

;; [[file:~/.emacs.d/readme.org::*rust][rust:1]]
(use-package rust-mode
  :mode "\\.rs\\'"
  :init
  (setq rust-format-on-save t))

(use-package lsp-rust
  :after lsp-mode)
;; rust:1 ends here

;; [[file:~/.emacs.d/readme.org::*sql][sql:1]]
;; Make SQLi default to PostgreSQL syntax highlighti
;; https://blogs.gentoo.org/titanofold/2011/05/17/postgresql-syntax-highlighting-in-emacs/
(eval-after-load "sql"
  '(progn (sql-set-product 'postgres)))

;; Set default config for login
;; https://truongtx.me/2014/08/23/setup-emacs-as-an-sql-database-client/
(setq sql-postgres-login-params
      '((user :default "postgres")
        (database :default "postgres")
        (server)))

;; Truncate lines to better visualize many columns tables
;; Automatically renames the *SQL* buffer to *SQL user/database*
(add-hook 'sql-interactive-mode-hook
          (lambda ()
            (toggle-truncate-lines t)
            (sql-rename-buffer)))

(use-package sqlup-mode
  :bind (("C-c u" . sqlup-capitalize-keywords-in-region))
  :config
  (add-hook 'sql-mode-hook 'sqlup-mode)                ; capitalize keywords in SQL mode
  (add-hook 'sql-interactive-mode-hook 'sqlup-mode))   ; and in an interactive session (e.g. psql)

(use-package sql-indent
  :config
  (add-hook 'sql-mode-hook 'sqlind-minor-mode))
;; sql:1 ends here

;; [[file:~/.emacs.d/readme.org::*eshell][eshell:1]]
(setenv "LANG" "en_US.UTF-8")
(setenv "PAGER" "cat")
(setq eshell-scroll-to-bottom-on-input t)

;; Define a keybinding to get to your eshell quickly.
(global-set-key (kbd "C-c e") 'eshell)

;; Visual commands are commands which require a proper terminal.
;; eshell will run them in a term buffer when you invoke them.
(setq eshell-visual-commands
      '("less" "tmux" "htop" "top" "bash" "zsh" "fish"))
(setq eshell-visual-subcommands
      '(("git" "log" "l" "diff" "show")))

(setq eshell-cmpl-cycle-completions t)  ;TAB for suggestion

(add-hook 'eshell-mode-hook
          (lambda ()
            (setq-local show-trailing-whitespace nil)
            (semantic-mode -1)
            (hl-line-mode -1)
            (global-hl-line-mode -1)))

;; Define a pretty prompt.
(use-package eshell-git-prompt
  :config
  (eshell-git-prompt-use-theme 'powerline))

(add-hook 'eshell-mode-hook
          (lambda ()
            (define-key eshell-mode-map
              (kbd "<tab>") 'completion-at-point)))


;; Like Plan-9 shell
(use-package em-smart
  :ensure nil
  :init
  (add-hook 'eshell-mode-hook 'eshell-smart-initialize)
  :config
  (setq eshell-where-to-jump 'begin
        eshell-review-quick-commands nil
        eshell-smart-space-goes-to-end t))

(use-package eshell-bookmark
  :hook (eshell-mode . eshell-bookmark-setup))
;; eshell:1 ends here

;; [[file:~/.emacs.d/readme.org::*better-shell][better-shell:1]]
(use-package better-shell
  :bind (("C-'" . better-shell-shell)
         ("C-;" . better-shell-remote-open)))
;; better-shell:1 ends here

;; [[file:~/.emacs.d/readme.org::*eshell%20history%20with%20counsel][eshell history with counsel:1]]
(defun mu-counsel-esh-history ()
  "Browse Eshell history."
  (interactive)
  (setq ivy-completion-beg (point))
  (setq ivy-completion-end (point))
  (ivy-read "Symbol name: "
            (delete-dups
             (ring-elements eshell-history-ring))
            :action #'ivy-completion-in-region-action))

(add-hook 'eshell-mode-hook
          #'(lambda ()
              (bind-key "C-c C-l" #'mu-counsel-esh-history
                        eshell-mode-map)))
;; eshell history with counsel:1 ends here

;; [[file:~/.emacs.d/readme.org::*tramp][tramp:1]]
(setq tramp-default-method "ssh")
;; tramp:1 ends here

;; [[file:~/.emacs.d/readme.org::*ansible][ansible:1]]
(use-package ansible
  :init
  (add-hook 'yaml-mode-hook '(lambda () (ansible 1))))

(use-package ansible-doc
  :after ansible
  :init
  (add-hook 'yaml-mode-hook #'ansible-doc-mode))
;; ansible:1 ends here

;; [[file:~/.emacs.d/readme.org::*docker][docker:1]]
(use-package dockerfile-mode)

(use-package docker-compose-mode)

(use-package docker-tramp)
;; docker:1 ends here

;; [[file:~/.emacs.d/readme.org::*puppet][puppet:1]]
(use-package puppet-mode)
;; puppet:1 ends here

;; [[file:~/.emacs.d/readme.org::*vagrant][vagrant:1]]
(use-package vagrant)

(use-package vagrant-tramp
  :after vagrant
  :config
  (eval-after-load 'tramp '(vagrant-tramp-add-method)))
;; vagrant:1 ends here

;; [[file:~/.emacs.d/readme.org::*ztree][ztree:1]]
(use-package ztree)
;; ztree:1 ends here

;; [[file:~/.emacs.d/readme.org::*Main%20configuration][Main configuration:1]]
(use-package dired
  :ensure nil
  :config
  (validate-setq
   ;;ls-lisp-dirs-first t
   ;; this is only for macOS - FIXME
   insert-directory-program "gls"
   dired-use-ls-dired t
   dired-listing-switches
   "-lhFG1v --group-directories-first"  ; add ls switches

   dired-ls-F-marks-symlinks t           ; -F marks links with @
   dired-dwim-target t                   ; when in a split windows, use other pane as target
   dired-recursive-copies 'always        ; copy dirs recursively
   dired-recursive-deletes 'top          ; ask before deleting dirs recursively
   delete-by-moving-to-trash t           ; don't delete files outright
   dired-auto-revert-buffer t            ; revert buffers on revisiting
   )

  (setq wdired-allow-to-change-permissions t)  ; allow editing file permissions

  (add-hook 'dired-mode-hook #'toggle-truncate-lines)   ; handle long file names
  (add-hook 'dired-mode-hook 'auto-revert-mode)         ; auto refresh dired when file changes
  )

;; Built into Emacs, it provides chgrp, chown, chmod and other functionalities
(use-package dired-aux
  :ensure nil)
;; Main configuration:1 ends here

;; [[file:~/.emacs.d/readme.org::*dired-x][dired-x:1]]
(use-package dired-x
  :ensure nil
  :bind ("C-x C-j" . dired-jump)
  :config
  (validate-setq
   dired-clean-up-buffers-too t)          ; kill buffer of files/dir that are deleted in dired

  (add-hook 'dired-mode-hook (lambda () (dired-omit-mode)))
  (add-to-list 'dired-omit-extensions ".DS_Store"))
;; dired-x:1 ends here

;; [[file:~/.emacs.d/readme.org::*dired-async][dired-async:1]]
(use-package dired-async
  :ensure nil
  :after async
  :config
  (dired-async-mode t)
  (autoload 'dired-async-mode "dired-async.el" nil t))
;; dired-async:1 ends here

;; [[file:~/.emacs.d/readme.org::*dired-quick-sort][dired-quick-sort:1]]
(use-package dired-quick-sort
  :config
  (dired-quick-sort-setup))
;; dired-quick-sort:1 ends here

;; [[file:~/.emacs.d/readme.org::*dired-narrow][dired-narrow:1]]
(use-package dired-narrow
  :bind (:map dired-mode-map
              ("/" . dired-narrow)))
;; dired-narrow:1 ends here

;; [[file:~/.emacs.d/readme.org::*dired-rainbow][dired-rainbow:1]]
(use-package dired-rainbow)
;; dired-rainbow:1 ends here

;; [[file:~/.emacs.d/readme.org::*peep-dired][peep-dired:1]]
(use-package peep-dired
  :bind (:map dired-mode-map
              ("P" . peep-dired))
  :config
  (validate-setq peep-dired-ignored-extensions '("mkv" "iso" "mp4")))
;; peep-dired:1 ends here

;; [[file:~/.emacs.d/readme.org::*Icons%20in%20Dired%20buffers%20(and%20other%20buffers)][Icons in Dired buffers (and other buffers):1]]
(use-package all-the-icons-dired
  :after all-the-icons
  :init
  (add-hook 'dired-mode-hook 'all-the-icons-dired-mode))
;; Icons in Dired buffers (and other buffers):1 ends here

;; [[file:~/.emacs.d/readme.org::*Rename%20file%20from%20space%20to%20hyphen/underscore][Rename file from space to hyphen/underscore:1]]
(defun xah-dired-rename-space-to-underscore ()
  "In dired, rename current or marked files by replacing space to underscore _.
If not in `dired', do nothing.
URL `http://ergoemacs.org/emacs/elisp_dired_rename_space_to_underscore.html'
Version 2017-01-02"
  (interactive)
  (require 'dired-aux)
  (if (equal major-mode 'dired-mode)
      (progn
        (mapc (lambda (x)
                (when (string-match " " x )
                  (dired-rename-file x (replace-regexp-in-string " " "_" x) nil)))
              (dired-get-marked-files ))
        (revert-buffer))
    (user-error "Not in dired.")))

(defun xah-dired-rename-space-to-hyphen ()
  "In dired, rename current or marked files by replacing space to hyphen -.
If not in `dired', do nothing.
URL `http://ergoemacs.org/emacs/elisp_dired_rename_space_to_underscore.html'
Version 2016-12-22"
  (interactive)
  (require 'dired-aux)
  (if (equal major-mode 'dired-mode)
      (progn
        (mapc (lambda (x)
                (when (string-match " " x )
                  (dired-rename-file x (replace-regexp-in-string " " "_" x) nil)))
              (dired-get-marked-files ))
        (revert-buffer))
    (user-error "Not in dired")))
;; Rename file from space to hyphen/underscore:1 ends here

;; [[file:~/.emacs.d/readme.org::*Calculate%20size%20of%20directory][Calculate size of directory:1]]
(defun dired-get-size ()
  (interactive)
  (let ((files (dired-get-marked-files)))
    (with-temp-buffer
      (apply 'call-process "/usr/bin/du" nil t nil "-sch" files)
      (message
       "Size of all marked files: %s"
       (progn
         (re-search-backward "\\(^[ 0-9.,]+[A-Za-z]+\\).*total$")
         (match-string 1))))))
;; Calculate size of directory:1 ends here

;; [[file:~/.emacs.d/readme.org::*define-word][define-word:1]]
(use-package define-word)
;; define-word:1 ends here

;; [[file:~/.emacs.d/readme.org::*powerthesaurus][powerthesaurus:1]]
(use-package powerthesaurus)
;; powerthesaurus:1 ends here

;; [[file:~/.emacs.d/readme.org::*IRC%20(Internet%20Relay%20Chat)][IRC (Internet Relay Chat):1]]
(use-package erc
  :config
  (progn
    (erc-track-mode t)                      ; track activities on chats
    (erc-truncate-mode +1)                  ; truncate long irc buffers
    (erc-spelling-mode 1)                   ; enable spell checking

    (setq erc-interpret-mirc-color t        ; interpret mIRC-style color commands in IRC chats
          erc-kill-buffer-on-part t         ; kill buffer for channels after /part
          erc-kill-queries-on-quit t        ; kill buffer for private queries after quitting the server
          erc-kill-server-buffer-on-quit t  ; kill buffer for server messages after quitting the server
          erc-query-display 'buffer         ; open query buffers in the current window
          erc-save-buffer-on-part t         ; logging

          ;; autoaway setup
          erc-auto-discard-away t
          erc-autoaway-idle-seconds 600
          erc-autoaway-use-emacs-idle t

          erc-server-coding-system '(utf-8 . utf-8) ; utf-8 always and forever

          erc-track-exclude-types '("JOIN" "NICK" "PART" "QUIT" "MODE"
                                    "324" "329" "332" "333" "353" "477") ; exclude these from tracking
          erc-hide-list '("JOIN" "PART" "QUIT" "NICK")  ; doesn't show any of these
          )

  ;; logging
  ;; (setq erc-log-channels-directory "~/.erc/logs/")

  ;; (if (not (file-exists-p erc-log-channels-directory))
  ;;     (mkdir erc-log-channels-directory t))

  (defvar erc-notify-nick-alist nil
    "Alist of nicks and the last time they tried to trigger a
  notification")

  (defvar erc-notify-timeout 10
    "Number of seconds that must elapse between notifications from
  the same person.")

  (defun erc-notify-allowed-p (nick &optional delay)
    "Return non-nil if a notification should be made for NICK.
  If DELAY is specified, it will be the minimum time in seconds
  that can occur between two notifications.  The default is
  `erc-notify-timeout'."
    (unless delay (setq delay erc-notify-timeout))
    (let ((cur-time (time-to-seconds (current-time)))
          (cur-assoc (assoc nick erc-notify-nick-alist))
          (last-time nil))
      (if cur-assoc
          (progn
            (setq last-time (cdr cur-assoc))
            (setcdr cur-assoc cur-time)
            (> (abs (- cur-time last-time)) delay))
        (push (cons nick cur-time) erc-notify-nick-alist)
        t)))

  (defun start-irc ()
    "Connect to IRC."
    (interactive)
    (when (y-or-n-p "Do you want to start IRC? ")
      (erc :server "irc.freenode.net"
           :port 6667
           :nick rubens)))

  ;; (defun erc-start-or-switch ()
  ;;   "Connect to ERC, or switch to last active buffer"
  ;;   (interactive)
  ;;   (if (get-buffer "irc.freenode.net:6667")  ; ERC already active?
  ;;       (erc-track-switch-buffer 1)           ; yes: switch to last active

  ;;     (when (y-or-n-p "Start ERC? ")          ; no: maybe start ERC
  ;;       (erc :server "irc.freenode.net"
  ;;            :port 6667
  ;;            :nick "rsouza"))))

  (defun filter-server-buffers ()
    (delq nil
          (mapcar
           (lambda (x) (and (erc-server-buffer-p x) x))
           (buffer-list))))

  (defun stop-irc ()
    "Disconnects from all irc servers"
    (interactive)
    (dolist (buffer (filter-server-buffers))
      (message "Server buffer: %s" (buffer-name buffer))
      (with-current-buffer buffer
        (erc-quit-server "Asta la vista"))))

  (setq erc-autojoin-channels-alist '(("freenode.net"
                                       "#org-mode"
                                       "#hacklabto"
                                       "#emacs"
                                       "#itpug-soci")))

  (require 'erc-log)
  (require 'erc-notify)
  (require 'erc-spelling)
  (require 'erc-autoaway)
  ))
;; IRC (Internet Relay Chat):1 ends here

;; [[file:~/.emacs.d/readme.org::*hunspell%20setup][hunspell setup:1]]
(use-package ispell
  :if
  (and (eq system-type 'darwin)
       (setenv "DICPATH" "/Users/rubens/Library/Spelling")
       (setenv "DICTIONARY" "en_GB")
       (setenv "LANG" "en_GB"))
  :config
  (setq ispell-dictionary "en_GB")
  (setq ispell-program-name (executable-find "hunspell"))
  (setq ispell-extra-args '("-i" "utf-8"))
  (setq ispell-really-hunspell t)
  (setq ispell-check-comments  t)

  (unless ispell-program-name
    (warn "No spell checker available. Please install hunspell.")))

(use-package flyspell
  :defer t
  :hook ((prog-mode     . flyspell-prog-mode)
         (text-mode     . flyspell-mode)
         (org-mode      . flyspell-mode)
         (markdown-mode . flyspell-mode)
         (LaTeX-mode    . flyspell-mode))
  :config
  (progn
    (setq ispell-local-dictionary-alist '(("en_GB"
                                           "[[:alpha:]]"
                                           "[^[:alpha:]]"
                                           "[']" nil
                                           ("-d" "en_GB") nil utf-8)
                                          ("it_IT"
                                           "[[:alpha:]]"
                                           "[^[:alpha:]]"
                                           "[']" nil
                                           ("-d" "it_IT") nil utf-8)
                                          )))

  (setq flyspell-issue-welcome-flag nil  ; turn off flyspell welcome message
        flyspell-issue-message-flag nil) ; turn off flyspell messages when checking words
)

(use-package flyspell-correct-ivy       ; better interface for corrections
  :demand t
  :bind (:map flyspell-mode-map
              ("C-c $" . flyspell-correct-at-point)))
;; hunspell setup:1 ends here

;; [[file:~/.emacs.d/readme.org::*Switch%20dictionaries][Switch dictionaries:1]]
(defun switch-dictionary ()
  (interactive)
  (let* ((dic ispell-current-dictionary)
         (change (if (string= dic "en_GB") "it_IT" "en_GB")))
    (ispell-change-dictionary change)
    (message "Dictionary switched from %s to %s" dic change)))

(global-set-key (kbd "<f8>") 'switch-dictionary)
;; Switch dictionaries:1 ends here

;; [[file:~/.emacs.d/readme.org::*flyspell-lazy][flyspell-lazy:1]]
(use-package flyspell-lazy
  :after flyspell
  :config
  (flyspell-lazy-mode 1))
;; flyspell-lazy:1 ends here

;; [[file:~/.emacs.d/readme.org::*flyspell-popup][flyspell-popup:1]]
(use-package flyspell-popup
  :after flyspell
  :bind (:map flyspell-mode-map
              ("C-;" . flyspell-popup-correct)))
;; flyspell-popup:1 ends here

;; [[file:~/.emacs.d/readme.org::*languagetool][languagetool:1]]
(use-package langtool
  :bind (("C-x 4 w" . langtool-check)                   ; check buffer and show warnings
         ("C-x 4 W" . langtool-check-done)              ; finish checking and remove markers
         ("C-x 4 l" . langtool-switch-default-language) ; swicth languages
         ("C-x 4 n" . langtool-goto-next-error)         ; go to the next error
         ("C-x 4 4" . langtool-show-message-at-point)   ; show the warning at point
         ("C-x 4 c" . langtool-correct-buffer)          ; correct markers
         )
  :config
  (cond ((system-is-linux)
         (validate-setq
          langtool-language-tool-jar "/usr/share/java/languagetool/languagetool-commandline.jar"))
        ((system-is-mac)
         (validate-setq
          langtool-language-tool-jar "/usr/local/Cellar/languagetool/3.9/libexec/languagetool-commandline.jar")))

  (validate-setq langtool-java-bin "/usr/bin/java"
                 langtool-mother-tongue "en")

  (setq langtool-disabled-rules '("WHITESPACE_RULE"
                                  "EN_UNPAIRED_BRACKETS"
                                  "COMMA_PARENTHESIS_WHITESPACE"
                                  "EN_QUOTES"))

  ;; show suggestions in a popup
  (defun langtool-autoshow-detail-popup (overlays)
    (when (require 'popup nil t)
      ;; Do not interrupt current popup
      (unless (or popup-instances
                  ;; suppress popup after type `C-g` .
                  (memq last-command '(keyboard-quit)))
        (let ((msg (langtool-details-error-message overlays)))
          (popup-tip msg)))))

  (validate-setq langtool-autoshow-message-function
                 'langtool-autoshow-detail-popup))
;; languagetool:1 ends here

;; [[file:~/.emacs.d/readme.org::*writegood][writegood:1]]
(use-package writegood-mode
  :disabled t
  :config
  (progn
    (add-hook 'org-mode-hook      'writegood-mode)
    (add-hook 'text-mode-hook     'writegood-mode)
    (add-hook 'markdown-mode-hook 'writegood-mode)))
;; writegood:1 ends here

;; [[file:~/.emacs.d/readme.org::*pdf-tools][pdf-tools:1]]
;; (setq-default TeX-master nil)
;; (setq TeX-parse-self t)
;; (setq TeX-auto-save t)
;; (setq TeX-save-query nil)

;; ;; revert pdf-view after compilation
;;(add-hook 'TeX-after-compilation-finished-functions #'TeX-revert-document-buffer)

;; set PKG_CONFIG_PATH variable in order to compile pdf-tools successfully on MacOS
(when (system-is-mac)
  (setenv "PKG_CONFIG_PATH" "/opt/X11/lib/pkgconfig"))

(use-package pdf-tools
  :config
  (pdf-tools-install)
  (setq TeX-view-program-selection '((output-pdf "pdf-tools"))
        ;;TeX-view-program-selection '((output-pdf "PDF Tools"))
        TeX-view-program-list '(("pdf-tools" "TeX-pdf-tools-sync-view"))
        TeX-source-correlate-start-server t)

  (setq-default pdf-view-display-size 'fit-page)
  (bind-keys :map pdf-view-mode-map
             ("\\" . hydra-pdftools/body)
             ("<s-spc>" .  pdf-view-scroll-down-or-next-page)
             ("g"  . pdf-view-first-page)
             ("G"  . pdf-view-last-page)
             ("l"  . image-forward-hscroll)
             ("h"  . image-backward-hscroll)
             ("j"  . pdf-view-next-page)
             ("k"  . pdf-view-previous-page)
             ("e"  . pdf-view-goto-page)
             ("u"  . pdf-view-revert-buffer)
             ("al" . pdf-annot-list-annotations)
             ("ad" . pdf-annot-delete)
             ("aa" . pdf-annot-attachment-dired)
             ("am" . pdf-annot-add-markup-annotation)
             ("at" . pdf-annot-add-text-annotation)
             ("y"  . pdf-view-kill-ring-save)
             ("i"  . pdf-misc-display-metadata)
             ("s"  . pdf-occur)
             ("b"  . pdf-view-set-slice-from-bounding-box)
             ("r"  . pdf-view-reset-slice)))

(use-package org-pdfview
  :after pdf-tools
  :config
  (add-to-list 'org-file-apps
               '("\\.pdf\\'" . (lambda (file link)
                                 (org-pdfview-open link)))))

;; hydra

(defhydra hydra-pdftools (:color blue :hint nil)
  "
                                                                      ╭───────────┐
       Move  History   Scale/Fit     Annotations  Search/Link    Do   │ PDF Tools │
   ╭──────────────────────────────────────────────────────────────────┴───────────╯
         ^^_g_^^      _B_    ^↧^    _+_    ^ ^     [_al_] list    [_s_] search    [_u_] revert buffer
         ^^^↑^^^      ^↑^    _H_    ^↑^  ↦ _W_ ↤   [_am_] markup  [_o_] outline   [_i_] info
         ^^_p_^^      ^ ^    ^↥^    _0_    ^ ^     [_at_] text    [_F_] link      [_d_] dark mode
         ^^^↑^^^      ^↓^  ╭─^─^─┐  ^↓^  ╭─^ ^─┐   [_ad_] delete  [_f_] search link
    _h_ ←pag_e_→ _l_  _N_  │ _P_ │  _-_    _b_     [_aa_] dired
         ^^^↓^^^      ^ ^  ╰─^─^─╯  ^ ^  ╰─^ ^─╯   [_y_]  yank
         ^^_n_^^      ^ ^  _r_eset slice box
         ^^^↓^^^
         ^^_G_^^
   --------------------------------------------------------------------------------
        "
  ("\\" hydra-master/body "back")
  ("<ESC>" nil "quit")
  ("al" pdf-annot-list-annotations)
  ("ad" pdf-annot-delete)
  ("aa" pdf-annot-attachment-dired)
  ("am" pdf-annot-add-markup-annotation)
  ("at" pdf-annot-add-text-annotation)
  ("y"  pdf-view-kill-ring-save)
  ("+" pdf-view-enlarge :color red)
  ("-" pdf-view-shrink :color red)
  ("0" pdf-view-scale-reset)
  ("H" pdf-view-fit-height-to-window)
  ("W" pdf-view-fit-width-to-window)
  ("P" pdf-view-fit-page-to-window)
  ("n" pdf-view-next-page-command :color red)
  ("p" pdf-view-previous-page-command :color red)
  ("d" pdf-view-dark-minor-mode)
  ("b" pdf-view-set-slice-from-bounding-box)
  ("r" pdf-view-reset-slice)
  ("g" pdf-view-first-page)
  ("G" pdf-view-last-page)
  ("e" pdf-view-goto-page)
  ("o" pdf-outline)
  ("s" pdf-occur)
  ("i" pdf-misc-display-metadata)
  ("u" pdf-view-revert-buffer)
  ("F" pdf-links-action-perfom)
  ("f" pdf-links-isearch-link)
  ("B" pdf-history-backward :color red)
  ("N" pdf-history-forward :color red)
  ("l" image-forward-hscroll :color red)
  ("h" image-backward-hscroll :color red))
;; pdf-tools:1 ends here

;; [[file:~/.emacs.d/readme.org::*main%20theme][main theme:1]]
(use-package spacemacs-theme
  :disabled t
  :no-require t
  :init
  (load-theme 'spacemacs-light t))

(use-package doom-themes
  :init
  (setq doom-themes-enable-bold t     ; bold is universally enable
        doom-themes-enable-italic t)  ; italics is universally enabled
  (load-theme 'doom-one t)
  :config
  (doom-themes-visual-bell-config)  ; flashes mode-line on errors
  (doom-themes-org-config))         ; corrects (and improves) org-mode's native fontification

(use-package leuven-theme
  :disabled t
  :init
  (load-theme 'leuven t))

(use-package gruvbox-theme
  :disabled t
  :config
  (load-theme 'gruvbox-light-soft t))

(use-package poet-theme
  :disabled t
  :init
  (load-theme 'poet t)
  :config
  (add-to-list 'default-frame-alist '(ns-transparent-titlebar . t))
  (add-to-list 'default-frame-alist '(ns-appearance . light)))
;; main theme:1 ends here

;; [[file:~/.emacs.d/readme.org::*%5B%5Bhttps://github.com/hlissner/emacs-solaire-mode%5D%5Bsolaire-mode%5D%5D][[[https://github.com/hlissner/emacs-solaire-mode][solaire-mode]]:1]]
(use-package solaire-mode
  :hook (((after-change-major-mode after-revert) . turn-on-solaire-mode)
         (ediff-prepare-buffer . solaire-mode)
         (minibuffer-setup . solaire-mode-in-minibuffer))
  :config
  (solaire-mode-swap-bg)
  (advice-add #'persp-load-state-from-file
              :after #'solaire-mode-restore-persp-mode-buffers))
;; [[https://github.com/hlissner/emacs-solaire-mode][solaire-mode]]:1 ends here

;; [[file:~/.emacs.d/readme.org::*Modeline][Modeline:1]]
;; (use-package spaceline)

;; (use-package spaceline-config
;;   :ensure nil
;;   :config
;;   (spaceline-spacemacs-theme))

(use-package spaceline
  :disabled t
  :config
  (progn
    (require 'spaceline-config)
    (spaceline-spacemacs-theme)
    (validate-setq powerline-default-separator 'wave)
    (spaceline-toggle-minor-modes-off)
    (spaceline-compile)
    ))

(use-package spaceline-all-the-icons
  :disabled t
  ;; :after spaceline
  :config
  (spaceline-all-the-icons-theme)
  (spaceline-all-the-icons--setup-anzu)      ; enable anzu searching
  (spaceline-all-the-icons--setup-git-ahead) ; enable # of commits ahead of upstream in git
  )

(use-package doom-modeline
  ;; :disabled t
  :hook (after-init . doom-modeline-init))
;; Modeline:1 ends here

;; [[file:~/.emacs.d/readme.org::*test%202][test 2:1]]
(cond ((system-is-linux)
       (set-face-attribute 'default nil
                           ;;:family "Source Code Pro"
                           :family "SauceCodePro Nerd Font"
                           :height 90))
      ((system-is-mac)
       (set-face-attribute 'default nil
                           ;;:family "FuraCode Nerd Font"
                           :family "SauceCodePro Nerd Font"
                           :height 110)

       ;; Enable emoji, and stop the UI from freezing when trying to display them.
       ;;(if (fboundp 'set-fontset-font)
       ;;    (set-fontset-font t 'unicode "Apple Color Emoji" nil 'prepend))
       ))

;; Set a font with great support for Unicode Symbols to fallback in those case
;; where certain Unicode glyphs are missing in the current font.
;; Test range: 🐷 ❤ ⊄ ∫ 𝛼 α 🜚 Ⓚ
;;(set-fontset-font "fontset-default" nil
;;                 (font-spec :size 100 :name "SauceCodePro Nerd"))
;; test 2:1 ends here

;; [[file:~/.emacs.d/readme.org::*all-the-icons][all-the-icons:1]]
(use-package all-the-icons)
;; all-the-icons:1 ends here

;; [[file:~/.emacs.d/readme.org::*fontawesome][fontawesome:1]]
(use-package fontawesome)
;; fontawesome:1 ends here

;; [[file:~/.emacs.d/readme.org::*beacon][beacon:1]]
(use-package beacon
  :config
  (beacon-mode t)
  (setq beacon-push-mark 35
        beacon-color "#666600"))
;; beacon:1 ends here

;; [[file:~/.emacs.d/readme.org::*crux][crux:1]]
(use-package crux
  :general (("C-a"   . crux-move-beginning-of-line)
            ("C-c o" . crux-open-with)
            ("C-^"   . crux-top-join-line)
            ("S-<return>"   . crux-smart-open-line)
            ("C-S-<return>" . crux-smart-open-line-above)
            ;; ([(control shift up)]     . crux-move-line-up)
            ;; ([(control shift down)]   . crux-move-line-down)
            ([remap kill-whole-line]  . crux-kill-whole-line)
            ))
;; crux:1 ends here

;; [[file:~/.emacs.d/readme.org::*expand-region][expand-region:1]]
(use-package expand-region
  :commands (er/mark-word
             er/mark-defun
             er/mark-sentence)
  :bind ("C-=" . er/expand-region))
;; expand-region:1 ends here

;; [[file:~/.emacs.d/readme.org::*multiple-cursors][multiple-cursors:1]]
(use-package multiple-cursors
  :general
  ("M-s-s" 'mc/mark-previous-like-this
   "M-s-t" 'mc/mark-next-like-this
   "M-s-S" 'mc/unmark-next-like-this
   "M-s-T" 'mc/unmark-previous-like-this
   "H-m" 'hydra-mc/body)
  :commands
  (hydra-mc/mc/mark-previous-like-this
   hydra-mc/mc/mark-next-like-this)
  :config

  ;; from https://github.com/abo-abo/hydra/wiki/multiple-cursors
  (defhydra hydra-mc (:hint nil)
    "
     ^Up^            ^Down^        ^Other^
----------------------------------------------
[_p_]   Previous  [_n_]   Next    [_l_] Edit lines
[_P_]   Skip      [_N_]   Skip    [_a_] Mark all
[_M-p_] Unmark    [_M-n_] Unmark  [_r_] Mark by regexp
^ ^               ^ ^             [_q_] Quit
"
  ("l" mc/edit-lines :exit t)
  ("a" mc/mark-all-like-this :exit t)
  ("n" mc/mark-next-like-this)
  ("N" mc/skip-to-next-like-this)
  ("M-n" mc/unmark-next-like-this)
  ("p" mc/mark-previous-like-this)
  ("P" mc/skip-to-previous-like-this)
  ("M-p" mc/unmark-previous-like-this)
  ("r" mc/mark-all-in-region-regexp :exit t)
  ("q" nil :color blue)
  (" " nil "quit" :color blue)))
;; multiple-cursors:1 ends here

;; [[file:~/.emacs.d/readme.org::*zop-to-char][zop-to-char:1]]
(use-package zop-to-char
  :bind ([remap zap-to-char] . zop-up-to-char))
;; zop-to-char:1 ends here
