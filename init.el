
;; Code blocks on org mode can be tangled with =C-c C-v t=. This is not necessary
;; on this =readme.org= file as the tangling is done automatically.

;; The code block below is the responsable for creating the hook that will tangle a
;; new =~/.emacs.d/init.el= every time that this =readme.org= is saved. This also
;; means that alterations on the configuration have to be done on =readme.org=. Any
;; changes made on =~/.emacs.d/init.el= will be overwritten when =readme.org= is
;; saved.

;; originaly seen at
;; https://github.com/larstvei/dot-emacs/blob/master/init.org
(defun rts-tangle-init ()
  "If the current buffer is 'readme.org' the code-blocks are
tangled, and the tangled file is compiled."
  (when (equal (buffer-file-name)
               (expand-file-name (concat user-emacs-directory "readme.org")))
    (call-process-shell-command
     "emacs ~/.emacs.d/readme.org --batch --eval='(org-babel-tangle)' && notify-send -a 'Emacs' 'init file tangled'" nil 0)))
;;(byte-compile-file (concat user-emacs-directory "init.el")))

(add-hook 'after-save-hook 'rts-tangle-init)

;; Debugging

(setq message-log-max 10000)

;; Package management

;; Avoid accidentally using outdated compiled files
(setq load-prefer-newer t)

;; By default package-initialize is called after init.el.
;; Calling it here because some packages listed depend on it.
(package-initialize)

;; Assures package-initialize is not called again after init.el.
(setq package-enable-at-startup nil)

;; Sets the ELPA repositories from where packages are fetched
(add-to-list 'package-archives '("org" . "http://orgmode.org/elpa/") t)
(add-to-list 'package-archives '("melpa" . "https://melpa.org/packages/"))

;; Automatically compile Emacs Lisp libraries
;;(require 'auto-compile)
;;(auto-compile-on-load-mode)
;;(auto-compile-on-save-mode)

;; Bootstrap `use-package'
(unless (package-installed-p 'use-package)
  (package-refresh-contents)
  (package-install 'use-package))

(eval-when-compile
  (require 'use-package)
  (setq-default use-package-always-defer t
                use-package-always-ensure t))

(require 'subr-x)
(require 'time-date)

(use-package validate
  :demand t)

;; load libraries
(use-package s)
(use-package f)

;; Initialization

(when (version< emacs-version "25")
  (warn "This configuration needs Emacs 25, but this is %s!" emacs-version))

;; Disables calling default.el (default settings) after init.el
(setq inhibit-default-init t)

;; Environment fixup

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

;; Customization

;; Save the custom settings to a separated file, instead of inside the init.el
;; file, avoiding cluttering it.

(defconst rts-custom-file (locate-user-emacs-file "custom.el")
  "File used to store settings from Customization UI.")

(use-package cus-edit
  :ensure nil
  :init (load rts-custom-file 'no-error 'no-message)
  :config
  (setq custom-file rts-custom-file
        custom-buffer-done-kill nil            ; kill when existing
        custom-buffer-verbose-help nil         ; remove redundant help text
        ;; Show me the real variable name
        custom-unlispify-tag-names nil
        custom-unlispify-menu-entries nil))

;; Emacs server

;; Loads Emacs as a server, allowing it to answer to client calls coming from
;; ~emacsclient~.

(use-package server
  :init (server-mode))

;; sensible-defaults.el

;; Use [[https://github.com/hrs/sensible-defaults.el][sensible-defaults.el]] for some basic settings.

(load-file "~/git/sensible-defaults.el/sensible-defaults.el")
(sensible-defaults/use-all-settings)
(sensible-defaults/use-all-keybindings)

;; Auto-save and backup files

;; Set all temporary files (~auto-save-file~, ~backup~, ~tramp session~, etc) to be
;; saved into the temporary directory, which is to be located at =~/.emacs.d/tmp=.

;; [[https://github.com/hrs/sensible-defaults.el][sensible-defaults.el]] set ~auto-save-file~ and ~backup~ to be saved into the
;; =/tmp= directory (on Unix-like OSs). Here I set the other files that can be used
;; between sessions (tramp session, bookmarks, etc) to be saved into
;; =~/.emacs.d/tmp=, leaving =~/.emacs.d= nice and clean :)

;; create the temporal directory
(defvar tmp-directory (concat user-emacs-directory "tmp/"))
(unless (file-exists-p tmp-directory) (make-directory tmp-directory))

(setq temporary-file-directory (concat
                                user-emacs-directory "tmp/"))

;; Keep all backup and auto-save files in one directory
(setq backup-directory-alist '(("." . "~/.emacs.d/tmp/backups")))
(setq auto-save-list-file-prefix (concat tmp-directory "auto-save-list/"))
(setq auto-save-file-name-transforms '((".*" "~/.emacs.d/tmp/auto-save-list/" t)))

;; Store all temporary files in the tmp directory
(setq-default tramp-persistency-file-name (concat tmp-directory "tramp")
              bookmark-default-file (concat tmp-directory "bookmarks")
              semanticdb-default-save-directory (concat tmp-directory "semanticdb")
              url-configuration-directory (concat tmp-directory "url")
              eshell-directory-name (concat tmp-directory "eshell" ))

;; Better Defaults

;; Other personal preferences not covered by [[https://github.com/hrs/sensible-defaults.el][sensible-defaults.el]].

;; disable menu, tool-bar and scroll-bar
(when (window-system)
  (menu-bar-mode -1)
  (tool-bar-mode -1)
  (scroll-bar-mode -1))

(setq apropos-do-all t                        ; apropos commands perform more extensive searches than default
      x-select-enable-clipboard t             ; allows pasting selection outside of Emacs
      echo-keystrokes 0.1                     ; shows keystrokes in progress
      use-dialog-box nil                      ; don't use dialog when using mouse click
      line-spacing '0.10                      ; line height
      )

(blink-cursor-mode -1)                        ; turn of the blinking cursor
;;(fringe-mode '(1 . 1))                      ; thinner window divisions
(defalias 'list-buffers 'ibuffer)             ; use ibuffer by default
(global-hl-line-mode 1)                       ; highlight the current line
(global-visual-line-mode t)                   ; break long line of text
(global-prettify-symbols-mode 1)              ; prettify symbols (lambdas, etc)

(save-place-mode 1)                           ; save cursor position for opened files
(setq save-place-file
      (concat tmp-directory "places"))

(setq-default indicate-empty-lines t)         ; show empty lines at bottom of buffer
(when (not indicate-empty-lines)
  (toggle-indicate-empty-lines))

(setq-default indent-tabs-mode  nil           ; always indent with spaces
              default-tab-width 4
              c-basic-offset 4)

(set-terminal-coding-system  'utf-8)          ; make sure that UTF-8 is used everywhere
(set-keyboard-coding-system  'utf-8)
(set-language-environment    'utf-8)
(set-selection-coding-system 'utf-8)
(setq locale-coding-system   'utf-8)
(prefer-coding-system        'utf-8)
(set-input-method nil)

;; settings for the modeline
(column-number-mode t)                        ; shows column number on the modeline
(setq size-indication-mode t)
;;(which-function-mode 1)

;; silence the beep sound, and shows the alarm bell visually on the modeline
(setq ring-bell-function (lambda ()
                           (invert-face 'mode-line)
                           (run-with-timer 0.1 nil
                                           'invert-face 'mode-line)))

;; text wrapping at 80 columns by default (only text)
;; (add-hook 'text-mode-hook 'turn-on-auto-fill)
;; (add-hook 'text-mode-hook
;;           '(lambda() (set-fill-column 80)))

;; browser settings
(setq browse-url-browser-function 'browse-url-generic
      browse-url-generic-program "firefox")

;; Location
;; Set the calendar to current location.

(setq calendar-week-start-day  1
      calendar-latitude 43.8
      calendar-longitude 11.0
      calendar-location-name "Prato, Italy")

;; Holidays
;; Let Emacs know about holidays of the location.

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

;; OS compatibility

;; Check which OS for keeping compatibility.

(defun system-is-mac ()
  (interactive)
  (string-equal system-type "darwin"))

(defun system-is-linux ()
  (interactive)
  (string-equal system-type "gnu/linux"))

;; Settings in case OS is Mac
(if (system-is-mac)
    (toggle-frame-fullscreen))       ;; fullscreen

;; History

;; Maintain a history of past actions and a reasonable number of lists.

(setq-default history-length 1000)
(setq savehist-file (concat
                     tmp-directory "history")
      history-delete-duplicates t
      savehist-save-minibuffer-history 1
      savehist-additional-variables '(kill-ring
                                      search-ring regexp-search-ring))
(savehist-mode t)

;; Scrolling

(setq scroll-margin 0
      scroll-conservatively 100000
      scroll-preserve-screen-position 1
      mouse-wheel-scroll-amount '(1 ((shift) . 1))
      mouse-wheel-progressive-speed nil
      mouse-wheel-follow-mouse 't)

;; Useful functions

;; These functions are useful. Activate them.

(put 'downcase-region 'disabled nil)
(put 'upcase-region 'disabled nil)
(put 'narrow-to-region 'disabled nil)
(put 'narrow-to-page 'disabled nil)
(put 'dired-find-alternate-file 'disabled nil)

;; Customizing the customize
;; [[http://oremacs.com/2015/01/17/setting-up-ediff/][Oremacs source]] - This function is used in some point of this ~init.el~ file for
;; setting custom variables. Basically it is a ~setq~ that is aware of the
;; custom-set property of a variable.

(defmacro csetq (variable value)
  `(funcall (or (get ',variable 'custom-set)
                'set-default)
            ',variable ,value))

;; Split window and move

;; I hate the default Emacs behavior of split windows which just splits the window
;; but doesn’t go there.

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

;; Smarter start of line

;; This function, from [[http://emacsredux.com/blog/2013/05/22/smarter-navigation-to-the-beginning-of-a-line/][emacsredux]] blog, defines a better start of line and remaps
;; ~C-a~ for it.

(defun smarter-move-beginning-of-line (arg)
  "Move point back to indentation of beginning of line.

Move point to the first non-whitespace character on this line.
If point is already there, move to the beginning of the line.
Effectively toggle between the first non-whitespace character and
the beginning of the line.

If ARG is not nil or 1, move forward ARG - 1 lines first.  If
point reaches the beginning or end of the buffer, stop there."
  (interactive "^p")
  (setq arg (or arg 1))

  ;; Move lines first
  (when (/= arg 1)
    (let ((line-move-visual nil))
      (forward-line (1- arg))))

  (let ((orig-point (point)))
    (back-to-indentation)
    (when (= orig-point (point))
      (move-beginning-of-line 1))))

;; remap C-a to `smarter-move-beginning-of-line'
(global-set-key [remap move-beginning-of-line]
                'smarter-move-beginning-of-line)

(global-set-key (kbd "C-a") 'smarter-move-beginning-of-line)

;; Kill the current buffer

;; Change the key-binding to kill the current buffer instead of asking which one to
;; kill. Very good tip taken from [[http://pragmaticemacs.com/emacs/dont-kill-buffer-kill-this-buffer-instead/][Pragmaticemacs]].

(global-set-key (kbd "C-x k") 'kill-this-buffer)

;; Org header IDs

(defun my/copy-id-to-clipboard() "Copy the ID property value to killring,
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

;; general settings

(use-package org
  :ensure org-plus-contrib
  :bind  (("C-c a" . org-agenda-list)
          ("C-c c" . org-capture)
          ("C-c l" . org-store-link)
          ;;("C-c f" . org-cycle-agenda-files)
          ;;("C-c s" . org-search-view)
          ("C-c t" . org-todo-list))
  :config
  (validate-setq
   org-tags-column -80
   org-ellipsis "⤵"                        ; ⬎, ⤷, ⤵, ⚡
   org-fontify-whole-heading-line t        ; fontify the whole line for headings
   org-fontify-done-headline t
   org-fontify-quote-and-verse-blocks t
   org-startup-indented t
   org-cycle-include-plain-lists t
   org-list-allow-alphabetical t
   org-src-fontify-natively t              ; highlights code-blocks natively
   org-src-tab-acts-natively t             ; in code-blocks TAB acts natively language major mode
   org-src-window-setup 'current-window    ; open code-blocks in the current window
   org-confirm-babel-evaluate nil          ; don't ask for confirmation when compiling code-blocks
   org-latex-create-formula-image-program 'imagemagick)  ; preview latex fragments

  (validate-setq
   org-directory "~/org"
   org-default-notes-file (concat
                           org-directory "/notes.org")
   org-agenda-files (list "~/org/todo.org"
                          "~/org/2ndquadrant.org"))

  ;; Define TODO workflow states
  (validate-setq
   org-todo-keywords
   '("☛ TODO(t)" "⚑ WAIT(w@)" "|" "✔ DONE(d)" "✘ CANCEL(c@)"))

  (validate-setq
   org-todo-keyword-faces
   '(("☛ TODO"   . (:foreground "#ff4500" :weight bold))
     ("✔ DONE"   . (:foreground "#00ff7f" :weight bold))
     ("⚑ WAIT"   . (:foreground "#ffff00" :weight bold))
     ("✘ CANCEL" . (:foreground "#00bfff" :weight bold))))
  )

;; org-clock

(use-package org-clock
  :ensure org-plus-contrib
  :demand t
  :config
  (org-clock-persistence-insinuate)           ; resume clocking task when emacs is restarted
  (validate-setq
   org-clock-persist t                        ; save all clock history when exiting Emacs, load it on startup
   org-clock-persist-file                     ; where to save the persistent clock data
   (concat tmp-directory "org-clock-save.el")
   org-clock-persist-query-resume nil         ; do not prompt to resume an active clock
   org-clock-history-length 10                ; show lot of clocking history from where choose items
   org-clock-in-resume t                      ; resume clocking task on clock-in if the clock is open
   org-clock-into-drawer "CLOCKING"           ; clocking goes into specfic drawer
   org-clock-report-include-clocking-task t)) ; include current clocking task in clock reports

;; org-capture-templates

(use-package org-protocol
  :ensure org-plus-contrib
  :demand t
  :config
  ;; Define capture templates
  (setq org-capture-templates
        '(("w" "Web bookmarks" entry
           (file+headline (concat org-directory "/www.org") "Bookmarks")
           "* %?%c %^g\n:PROPERTIES:\n:CREATED: %U\n:END:\n%i\n"
           :empty-lines 1
           :immediate-finish)

          ("t" "Tasks" entry
           (file+headline (concat org-directory "/tasks.org") "Tasks")
           "* ☛ TODO %^{Task} %^g\n:PROPERTIES:\n:CREATED: %U\n:END:\n%?%i"
           :empty-lines 1)

          ("n" "Notes" entry
           (file+headline (concat org-directory "/notes.org") "Notes")
           "* %^{Header} %^G\n %u\n %?")

          ("j" "Journal" entry
           (file+datetree (concat org-directory "/journal.org"))
           "* %U %^{Title}\n %?%i\n %a")

          ("a" "Articles" entry
           (file+headline (concat org-directory "/articles.org") "Articles")
           "* %^{Title} %^g\n:PROPERTIES:\n:CREATED: %U\n:END:\n%?%i\n"
           :empty-lines 1
           :immediate-finish)

          ("r" "Redmine" entry
           (file+datetree (concat org-directory "/2ndQ.org"))
           "* [[https://redmine.2ndquadrant.it/issues/%^{Ticket}][%^{Description}]] :redmine:%^g\n%?"
           :clock-in t
           :clock-keep t
           :empty-lines 1)

          ("s" "RT - Support" entry
           (file+datetree (concat org-directory "/2ndQ.org"))
           "* [[https://support.2ndquadrant.com/rt/Ticket/Display.html?id=%^{Ticket}][%^{Description}]] :support:%^g\n%?"
           :clock-in t
           :clock-keep t
           :empty-lines 1)

          ("b" "RT - RDBA" entry
           (file+datetree (concat org-directory "/2ndQ.org"))
           "* [[https://support.2ndquadrant.com/rt/Ticket/Display.html?id=%^{Ticket}][%^{Description}]] :rdba:%^g\n%?"
           :clock-in t
           :clock-keep t
           :empty-lines 1)
          )))

;; org-bullets

(use-package org-bullets
  :demand t
  :after org-plug-contrib
  :config
  ;;(setq org-bullets-bullet-list '("☯" "☰" "☱" "☲" "☳" "☴" "☵" "☶" "☷"))
  ;;(setq org-bullets-bullet-list '("♣" "♥" "♠" "♦" "♧" "♡" "♤" "♢"))
  (validate-setq org-bullets-bullet-list '("☯" "☉" "∞" "◉" "⊚" "☀" "☾" "☥"))
  (add-hook 'org-mode-hook (lambda () (org-bullets-mode t)))

  ;; make available "org-bullet-face" such that I can control the font size individually
  (validate-setq org-bullets-face-name (quote org-bullet-face))
  (custom-set-faces '(org-bullet-face
                      ((t (:foreground "burlywood"
                                       :weight normal
                                       :height 1.6))))
                    ))

;; ox.el

(use-package ox
  :ensure org-plus-contrib
  :config
  (validate-setq org-export-with-smart-quotes t))

;; ox-pandoc

;; I’m using ox-pandoc to export org files to all formats Pandoc works with. It
;; only exports org files, in opposite of pandoc-mode, which exports from any
;; source format. The problem is that ox-pandoc needs considerably less
;; configuration and as I usually write everything in org-mode, no need to worry.
;; https://github.com/kawabata/ox-pandoc
;; http://www.rousette.org.uk/blog/archives/org-mode-and-pandoc/ Keeping a lab book
;; with org-mode http://informatica.boccaperta.com/m-x-emacs-ox-pandoc/

(use-package ox-pandoc
  :after org-plus-contrib
  :config
  (validate-setq org-pandoc-options '((standalone . t))            ; default options for all output formats
                 org-pandoc-options-for-docx '((standalone . nil)) ; cancel above settings only for 'docx' format
                 org-pandoc-options-for-beamer-pdf                 ; special settings for beamer-pdf
                 '((latex-engine . "lualatex"))
                 org-pandoc-options-for-latex-pdf                  ; and for latex-pdf exporters
                 '((latex-engine . "lualatex"))

                 ;; Use external css for html5
                 ;; (let ((stylesheet (expand-file-name
                 ;;                    (locate-user-emacs-file "etc/pandoc.css"))))
                 ;;   (setq org-pandoc-options-for-html5
                 ;;         `((css . ,(concat "file://" stylesheet)))))
                 )
  )

;; org-babel

;; Babel is Org-mode’s ability to execute source code within Org-mode documents.

(use-package ob
  :ensure org-plus-contrib
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
           (sh . t)
           (sqlite . t)
           (sql . t)
           ))))

;; main configuration

(use-package hydra
  :init
  (bind-key "\\" 'hydra-master/body)
  :config
  (setq lv-use-separator t)
  (set-face-attribute 'hydra-face-blue nil :foreground "deep sky blue" :weight 'bold)

  (eval-and-compile
    (defhydra hydra-common (:color blue)
      ("<ESC>" nil "quit")))

  (defhydra hydra-master (:color blue :idle 0.4)
    "
                                                                       ╭───────┐
                                                                       │ Index │
╭──────────────────────────────────────────────────────────────────────┴───────╯
  [_a_] bookmarks    [^h^]               [_o_] organization  [_v_] games
  [_b_] buffers      [_i_] internet      [_p_] project       [_w_] window
  [_c_] flycheck     [_j_] jump          [_q_] exit          [_x_] shell
  [_d_] development  [_k_] spell         [_r_] register      [^y^]
  [_e_] emacs        [_l_] lisp          [_s_] search        [^z^]
  [_f_] file         [_m_] media         [_t_] text
  [_g_] git          [_n_] narrow        [^u^]
--------------------------------------------------------------------------------
    "
    ("<SPC>" joe-alternate-buffers "alternate buffers")
    ("<ESC>" nil "quit")
    ("\\" (insert "\\") "\\")
    ("a"     hydra-bookmarks/body nil)
    ("b"     hydra-buffers/body nil)
    ("c"     hydra-flycheck/body nil)
    ("d"     hydra-development/body nil)
    ("e"     hydra-emacs/body nil)
    ("f"     hydra-file/body nil)
    ("g"     hydra-git/body nil)
    ("i"     hydra-internet/body nil)
    ("j"     hydra-jump/body nil)
    ("k"     hydra-spell/body nil)
    ("l"     hydra-lisp/body nil)
    ("m"     hydra-media/body nil)
    ("n"     hydra-narrow/body nil)
    ("o"     hydra-organization/body nil)
    ("p"     hydra-project/body nil)
    ("q"     hydra-exit/body nil)
    ("r"     hydra-register/body nil)
    ("s"     hydra-search/body nil)
    ("t"     hydra-text/body nil)
    ("v"     hydra-games/body nil)
    ("w"     ace-window nil)
    ("x"     hydra-system/body nil))

  (defhydra hydra-bookmarks (:color blue
                                    :hint nil
                                    :idle 0.4
                                    :inherit (hydra-common/heads))
    "
                                                                   ╭───────────┐
       List                          Do                            │ Bookmarks │
╭──────────────────────────────────────────────────────────────────┴───────────╯
  [_h_] list bookmarks (helm)     [_j_] jump to a bookmark
  [_l_] list bookmarks            [_m_] set bookmark at point
  ^ ^                             [_s_] save bookmarks
--------------------------------------------------------------------------------
    "
    ("h" helm-bookmarks)
    ("j" bookmark-jump)
    ("l" list-bookmarks)
    ("m" bookmark-set)
    ("s" bookmark-save))

  (defhydra hydra-buffers (:color blue
                                  :hint nil
                                  :idle 0.4
                                  :inherit (hydra-common/heads))
    "
                                                                     ╭─────────┐
  Switch                 Do                                          │ Buffers │
╭────────────────────────────────────────────────────────────────────┴─────────╯
  [_b_] switch (ido)       [_d_] kill the buffer
  [_i_] ibuffer            [_r_] toggle read-only mode
  [_a_] alternate          [_u_] revert buffer changes
  [_s_] switch (helm)      [_w_] save buffer
--------------------------------------------------------------------------------
    "
    ("a" joe-alternate-buffers)
    ("b" ivy-switch-buffer)
    ("d" joe-kill-this-buffer)
    ("i" ibuffer)
    ("m" ace-swap-window)
    ("r" read-only-mode)
    ("s" helm-buffers-list)
    ("u" joe-revert-buffer)
    ("w" save-buffer))

  (defhydra hydra-flycheck (:color blue
                                   :hint nil
                                   :idle 0.4
                                   :inherit (hydra-common/heads))
    "
                                                                    ╭──────────┐
   Navigate          Show Errors                  Do                │ Flycheck │
╭───────────────────────────────────────────────────────────────────┴──────────╯
   ^_p_^revious     [_l_] list errors           [_t_] toggle Flycheck
      ^^↑^^         [_e_] list errors (helm)    [_c_] select checker
    ^_f_^irst       [_d_] clear all errors      [_r_] run via compile
      ^^↓^^          ^ ^                        [_h_] describe checker
    ^_n_^ext
--------------------------------------------------------------------------------
      "
    ("c" flycheck-select-checker)
    ("h" flycheck-describe-checker)
    ("d" flycheck-clear)
    ("e" helm-flycheck)
    ("f" flycheck-first-error)
    ("l" flycheck-list-errors)
    ("n" flycheck-next-error :color red)
    ("p" flycheck-previous-error :color red)
    ("r" flycheck-compile)
    ("t" flycheck-mode))

  (defhydra hydra-development (:color blue
                                      :hint nil
                                      :idle 0.4
                                      :inherit (hydra-common/heads))
    "
                                                                 ╭─────────────┐
     Dash                   Web                 Quickrun         │ Development │
╭────────────────────────────────────────────────────────────────┴─────────────╯
  [_d_] search docs (at point) [_c_] Web Colors          [_q_] buffer
  [_D_] search docs            [_h_] HTTP header         [_v_] region
  [_i_] get docset             [_m_] HTTP method         [_x_] shell
  [_u_] get user docset        [_r_] HTTP relation       [_p_] with arg
  [_a_] activate docset        [_s_] HTTP status code    [_k_] buffer (helm)
   ^ ^                         [_g_] RESTclient          [_o_] only compile
   ^ ^                         [_f_] RFC doc             [_R_] replace
  [_l_] lines of code          [_F_] RFC index           [_e_] eval/print
--------------------------------------------------------------------------------
      "
    ("d" helm-dash-at-point)
    ("D" helm-dash)
    ("i" helm-dash-install-docset)
    ("u" helm-dash-install-user-docset)
    ("a" helm-dash-activate-docset)
    ("c" helm-colors)
    ("g" restclient-mode)
    ("f" irfc-visit)
    ("F" irfc-index)
    ("q" quickrun)
    ("v" quickrun-region)
    ("x" quickrun-shell)
    ("p" quickrun-with-arg)
    ("o" quickrun-compile-only)
    ("R" quickrun-replace-region)
    ("e" quickrun-eval-print)
    ("k" helm-quickrun)
    ("h" http-header)
    ("m" http-method)
    ("r" http-relation)
    ("s" http-status-code)
    ("l" cloc))

  (defhydra hydra-emacs (:color blue
                                :hint nil
                                :idle 0.4
                                :inherit (hydra-common/heads))
    "
                                                                       ╭───────┐
   Execute       Packages         Help                     Misc        │ Emacs │
╭──────────────────────────────────────────────────────────────────────┴───────╯
  [_s_] smex       [_p_] list      [_a_] apropos (helm)    [_t_] change theme (helm)
  [_m_] smex mode  [_i_] install   [_f_] info manual       [_l_] list emacs process
  [_h_] helm M-x   [_u_] upgrade   [_k_] bindings (helm)   [_c_] init time
  [_x_] counsel M-x ^ ^            [_b_] personal bindings [_o_] unbound commands
--------------------------------------------------------------------------------
      "
    ("C-h b" helm-descbinds "bindings")
    ("a" helm-apropos)
    ("b" describe-personal-keybindings)
    ("c" emacs-init-time)
    ("i" package-install)
    ("k" helm-descbinds)
    ("l" list-processes)
    ("f" info-display-manual)
    ("p" paradox-list-packages)
    ("t" helm-themes)
    ("u" paradox-upgrade-packages)
    ("m" smex-major-mode-commands)
    ("s" smex)
    ("h" helm-M-x)
    ("x" counsel-M-x)
    ("o" smex-show-unbound-commands))

  (defhydra hydra-file (:color blue
                               :hint nil
                               :idle 0.4
                               :inherit (hydra-common/heads))
    "
                                                                        ╭──────┐
     Ido               Helm                 Dired        Ztree          │ File │
╭───────────────────────────────────────────────────────────────────────┴──────╯
  [_o_] open file   [_f_] find file      [_d_] dired    [_z_] diff dirs
   ^ ^              [_m_] mini           [_r_] ranger
--------------------------------------------------------------------------------
      "
    ("o" find-file)
    ("f" helm-find-files)
    ("m" helm-mini)
    ("z" ztree-diff)
    ("d" dired)
    ("r" ranger))


  (defhydra hydra-text (:color blue
                               :hint nil
                               :idle 0.4
                               :inherit (hydra-common/heads))
    "
                                                                        ╭──────┐
 Size  Toggle              Unicode                        Do            │ Text │
╭───────────────────────────────────────────────────────────────────────┴──────╯
  _k_  [_f_] fill column     [_d_] unicode character           [_a_] align with regex
  ^↑^  [_h_] hidden chars    [_e_] evil digraphs table         [_w_] remove trailing ' '
  ^ ^  [_l_] line numbers    [_s_] specific code block         [_n_] count words
  ^↓^  [_t_] trailing ' '    [_u_] unicode character (helm)    [_i_] lorem ipsum
  _j_  [_v_] font space      [_p_] character code              [_x_] comment box
  ^ ^  [_c_] comment          ^ ^                              [_q_] boxquote
  ^ ^  [_b_] multibyte chars  ^ ^                              [_m_] iedit (multiple)
  ^ ^   ^ ^                   ^ ^                              [_r_] expand region
  ^ ^   ^ ^                   ^ ^                              [_U_] tabs to spaces
--------------------------------------------------------------------------------
      "
    ("a" align-regexp)
    ("b" toggle-enable-multibyte-characters)
    ("c" evilnc-comment-or-uncomment-lines)
    ("d" insert-char)
    ("e" evil-ex-show-digraphs)
    ("f" fci-mode)
    ("h" whitespace-mode)
    ("i" lorem-ipsum-insert-paragraphs)
    ("k" text-scale-increase :color red)
    ("j" text-scale-decrease :color red)
    ("l" linum-mode)
    ("n" count-words)
    ("m" iedit)
    ("p" describe-char)
    ("r" er/expand-region)
    ("s" char00map)
    ("t" joe-toggle-show-trailing-whitespace)
    ("u" helm-ucs)
    ("v" variable-pitch-mode)
    ("w" whitespace-cleanup)
    ("U" untabify)
    ("q" hydra-boxquote/body)
    ("x" comment-box))

  (defhydra hydra-git (:color blue
                              :hint nil
                              :idle 0.4
                              :inherit (hydra-common/heads))
    "
                                                                         ╭─────┐
   Magit                          VC                    Timemachine      │ Git │
╭────────────────────────────────────────────────────────────────────────┴─────╯
  [_s_] status              [_d_] diffs between revisions  [_t_] timemachine
  [_B_] blame mode          [_b_] edition history
  [_l_] file log
--------------------------------------------------------------------------------
      "
    ("B" magit-blame-mode)
    ("b" vc-annotate)
    ("d" vc-diff)
    ("l" magit-file-log)
    ("s" magit-status)
    ("t" git-timemachine))

  (defhydra hydra-internet (:color blue
                                   :hint nil
                                   :idle 0.4
                                   :inherit (hydra-common/heads))
    "
                                                                    ╭──────────┐
    Browse       Search             Social               Post       │ Internet │
╭───────────────────────────────────────────────────────────────────┴──────────╯
  [_w_] eww      [_g_] google          [_f_] elfeed            [_i_] imgur
  [_u_] url      [_m_] google maps     [_x_] stack overflow
   ^ ^           [_s_] surfraw
   ^ ^           [_d_] wordnik
--------------------------------------------------------------------------------
      "
    ("f" bjm/elfeed-load-db-and-open)
    ("g" google-this)
    ("i" imgur-post)
    ("m" google-maps)
    ("d" define-word-at-point)
    ("s" helm-surfraw)
    ("w" eww)
    ("u" browse-url-at-point)
    ("x" sx-tab-newest))

  (defhydra hydra-jump (:color blue
                               :hint nil
                               :idle 0.4
                               :inherit (hydra-common/heads))
    "
                                                                        ╭──────┐
  Window          Word/Char        Line         iSearch                 │ Jump │
╭───────────────────────────────────────────────────────────────────────┴──────╯
  [_w_] jump        [_j_] word         [_l_] jump     [_i_] jump
  [_d_] close       [_p_] all words    [_y_] copy
  [_z_] maximize    [_b_] subword      [_m_] move
  [_s_] swap        [_c_] char         [_v_] copy region
   ^ ^              [_a_] two chars
--------------------------------------------------------------------------------
      "
    ("w" ace-window)
    ("d" ace-delete-window)
    ("z" ace-maximize-window)
    ("s" ace-swap-window)
    ("j" avy-goto-word-1)
    ("p" avy-goto-word-0)
    ("b" avy-goto-subword-0)
    ("c" avy-goto-char)
    ("a" avy-goto-char-2)
    ("l" avy-goto-line)
    ("y" avy-copy-line)
    ("m" avy-move-line)
    ("v" avy-copy-region)
    ("i" avy-isearch))

  (defhydra hydra-spell (:color blue
                                :hint nil
                                :idle 0.4
                                :inherit (hydra-common/heads))
    "
                                                                       ╭───────┐
    Flyspell               Ispell                      Gtranslate      │ Spell │
╭──────────────────────────────────────────────────────────────────────┴───────╯
  [_k_] correct word       [_w_] check word            [_g_] en ⇆ es
  [_n_] next error         [_t_] toggle dictionary     [_G_] any lang
  [_f_] toggle flyspell    [_d_] change dictionary
  [_p_] toggle prog mode
--------------------------------------------------------------------------------
      "
    ("w" ispell-word)
    ("d" ispell-change-dictionary)
    ("t" joe-switch-dictionary)
    ("g" google-translate-smooth-translate)
    ("G" google-translate-query-translate)
    ("f" flyspell-mode)
    ("p" flyspell-prog-mode)
    ("k" flyspell-auto-correct-word)
    ("n" flyspell-goto-next-error))

  (defhydra hydra-lisp (:color blue
                               :hint nil
                               :idle 0.4
                               :inherit (hydra-common/heads))
    "
                                                                        ╭──────┐
    Elisp              Bug hunter                                       │ Lisp │
╭───────────────────────────────────────────────────────────────────────┴──────╯
  [_r_] eval region    [_f_] file
  [_s_] eval sexp      [_i_] init-file
--------------------------------------------------------------------------------
      "
    ("f" bug-hunter-file)
    ("i" bug-hunter-init-file)
    ("r" eval-region)
    ("s" eval-last-sexp))

  (defhydra hydra-narrow (:color blue
                                 :hint nil
                                 :idle 0.4
                                 :inherit (hydra-common/heads))
    "
                                                                      ╭────────┐
    Narrow                                                            │ Narrow │
╭─────────────────────────────────────────────────────────────────────┴────────╯
  [_f_] narrow to defun
  [_p_] narrow to page
  [_r_] narrow to region
  [_w_] widen
--------------------------------------------------------------------------------
      "
    ("f" narrow-to-defun)
    ("p" narrow-to-page)
    ("r" narrow-to-region)
    ("w" widen))

  (defhydra hydra-project (:color blue
                                  :hint nil
                                  :idle 0.4
                                  :inherit (hydra-common/heads))
    "
                                                                  ╭────────────┐
  Files             Search          Buffer             Do         │ Projectile │
╭─────────────────────────────────────────────────────────────────┴────────────╯
  [_f_] file          [_a_] ag          [_b_] switch         [_g_] magit
  [_l_] file dwim     [_A_] grep        [_v_] show all       [_p_] commander
  [_r_] recent file   [_s_] occur       [_V_] ibuffer        [_i_] info
  [_d_] dir           [_S_] replace     [_K_] kill all
  [_o_] other         [_t_] find tag
  [_u_] test file     [_T_] make tags
  [_h_] root
                                                                      ╭────────┐
  Other Window      Run             Cache              Do             │ Fixmee │
╭──────────────────────────────────────────────────╯ ╭────────────────┴────────╯
  [_F_] file          [_U_] test        [_kc_] clear         [_x_] TODO & FIXME
  [_L_] dwim          [_m_] compile     [_kk_] add current   [_X_] toggle
  [_D_] dir           [_c_] shell       [_ks_] cleanup
  [_O_] other         [_C_] command     [_kd_] remove
  [_B_] buffer
--------------------------------------------------------------------------------
      "
    ("a"   projectile-ag)
    ("A"   projectile-grep)
    ("b"   projectile-switch-to-buffer)
    ("B"   projectile-switch-to-buffer-other-window)
    ("c"   projectile-run-async-shell-command-in-root)
    ("C"   projectile-run-command-in-root)
    ("d"   projectile-find-dir)
    ("D"   projectile-find-dir-other-window)
    ("f"   projectile-find-file)
    ("F"   projectile-find-file-other-window)
    ("g"   projectile-vc)
    ("h"   projectile-dired)
    ("i"   projectile-project-info)
    ("kc"  projectile-invalidate-cache)
    ("kd"  projectile-remove-known-project)
    ("kk"  projectile-cache-current-file)
    ("K"   projectile-kill-buffers)
    ("ks"  projectile-cleanup-known-projects)
    ("l"   projectile-find-file-dwim)
    ("L"   projectile-find-file-dwim-other-window)
    ("m"   projectile-compile-project)
    ("o"   projectile-find-other-file)
    ("O"   projectile-find-other-file-other-window)
    ("p"   projectile-commander)
    ("r"   projectile-recentf)
    ("s"   projectile-multi-occur)
    ("S"   projectile-replace)
    ("t"   projectile-find-tag)
    ("T"   projectile-regenerate-tags)
    ("u"   projectile-find-test-file)
    ("U"   projectile-test-project)
    ("v"   projectile-display-buffer)
    ("V"   projectile-ibuffer)
    ("X"   fixmee-mode)
    ("x"   fixmee-view-listing))

  (defhydra hydra-exit (:color blue
                               :hint nil
                               :idle 0.4
                               :inherit (hydra-common/heads))
    "
                                                                        ╭──────┐
   Quit                                                                 │ Exit │
╭───────────────────────────────────────────────────────────────────────┴──────╯
  [_c_] exit emacs (standalone or client)
  [_s_] shutdown the emacs daemon
--------------------------------------------------------------------------------
      "
    ("c" save-buffers-kill-terminal)
    ("s" save-buffers-kill-emacs))

  (defhydra hydra-register (:color blue
                                   :hint nil
                                   :idle 0.4
                                   :inherit (hydra-common/heads))
    "
                                                                    ╭──────────┐
   Logs                        Registers                Undo        │ Register │
╭───────────────────────────────────────────────────────────────────┴──────────╯
  [_c_] commands history       [_e_] emacs registers    [_u_] undo tree
  [_o_] echo-area messages     [_r_] evil registers
  [_b_] minibuffer             [_m_] evil marks
  [_l_] messages               [_k_] kill ring
  [_d_] diff buffer with file
--------------------------------------------------------------------------------
      "
    ("c" helm-complex-command-history)
    ("d" joe-diff-buffer-with-file)
    ("e" helm-register)
    ("k" helm-show-kill-ring)
    ("a" helm-all-mark-rings)
    ("l" popwin:messages)
    ("m" evil-show-marks)
    ("o" view-echo-area-messages)
    ("r" evil-show-registers)
    ("b" helm-minibuffer-history)
    ("u" undo-tree-visualize))

  (defhydra hydra-search (:color blue
                                 :hint nil
                                 :idle 0.4
                                 :inherit (hydra-common/heads))
    "
                                                                      ╭────────┐
   Files                             Buffer                           │ Search │
╭─────────────────────────────────────────────────────────────────────┴────────╯
  [_a_] regex search (Ag)           [_b_] by word
  [_A_] regex by filetype (Ag)      [_o_] by word (occur)
  [_h_] regex search (grep & helm)  [_w_] by word (multi)
  [_g_] regex search (grep)         [_t_] tags & titles
  [_f_] find
  [_l_] locate
--------------------------------------------------------------------------------
      "
    ("A" ag-files)
    ("a" ag)
    ("b" helm-swoop)
    ("f" helm-find)
    ("g" rgrep)
    ("h" helm-do-grep)
    ("l" helm-locate)
    ("o" helm-occur)
    ("t" helm-semantic-or-imenu)
    ("w" helm-multi-swoop))

  (defhydra hydra-games (:color blue
                                :hint nil
                                :idle 0.4
                                :inherit (hydra-common/heads))
    "
                                                                       ╭───────┐
   Game                                                                │ Games │
╭──────────────────────────────────────────────────────────────────────┴───────╯
  [_b_] bubbles       [_c_] chess (computer)
  [_t_] tetris        [_a_] chess (internet)
  [_g_] gomoku
--------------------------------------------------------------------------------
      "
    ("b" bubbles-set-game-hard)
    ("c" chess)
    ("a" chess-ics)
    ("g" gomoku)
    ("t" tetris))

  (defhydra hydra-system (:color blue
                                 :hint nil
                                 :idle 0.4
                                 :inherit (hydra-common/heads))
    "
                                                                      ╭────────┐
   Terminals                     System                               │ System │
╭─────────────────────────────────────────────────────────────────────┴────────╯
  [_s_] new multi-term           [_c_] shell command
  [_n_] next multi-term          [_a_] aync shell command
  [_p_] previous multi-term      [_m_] man page
  [_d_] dedicated multi-term     [_l_] list system process
  [_e_] eshell                   [_t_] top command
--------------------------------------------------------------------------------
      "
    ("a" async-shell-command)
    ("c" shell-command)
    ("e" eshell)
    ("m" helm-man-woman)
    ("l" proced)
    ("s" multi-term)
    ("n" multi-term-next)
    ("p" multi-term-previous)
    ("d" multi-term-dedicated-toggle)
    ("t" helm-top))

  (defhydra hydra-media (:color blue
                                :hint nil
                                :idle 0.4
                                :inherit (hydra-common/heads))
    "
                                                                       ╭───────┐
   Mingus              Mpd                     Volume                  │ Media │
╭──────────────────────────────────────────────────────────────────────┴───────╯
 [_m_] mingus         [_n_] next song          [_-_] volume down
 [_f_] search         [_p_] previous song      [_+_] volume up
 [_l_] playlist       [_c_] clear playlist
 [_a_] All            [_t_] pause
  ^ ^                 [_s_] stop
  ^ ^                 [_d_] start daemon
--------------------------------------------------------------------------------
      "
    ("m" mingus)
    ("f" mingus-search)
    ("c" mingus-clear)
    ("n" mingus-next)
    ("p" mingus-prev)
    ("t" mingus-toggle)
    ("s" mingus-stop)
    ("d" mingus-start-daemon)
    ("l" mingus-load-playlist)
    ("a" mingus-load-all)
    ("-" mingus-vol-down)
    ("\+" mingus-vol-up))

  (defhydra hydra-organization (:color blue
                                       :hint nil
                                       :idle 0.4
                                       :inherit (hydra-common/heads))
    "
                                                                ╭──────────────┐
     Tasks            Org mode               Comms      Others  │ Organization │
╭───────────────────────────────────────────────────────────────┴──────────────╯
  [_a_] agenda      [_c_] capture             [_m_] mail      [_x_] speed type
  [_l_] agenda list [_p_] pomodoro            [_t_] contacts
  [_d_] calendar    [_s_] search headings     [_h_] add location
   ^ ^              [_g_] open location gmaps
   ^ ^              [_f_] archive subtree
--------------------------------------------------------------------------------
      "
    ("a" org-agenda)
    ("c" org-capture)
    ("d" cfw:open-org-calendar)
    ("g" org-location-google-maps)
    ("h" org-address-google-geocode-set)
    ("l" org-agenda-list)
    ("f" org-archive-subtree)
    ("m" mu4e)
    ("p" org-pomodoro)
    ("s" helm-org-agenda-files-headings)
    ("t" org-contacts)
    ("x" speed-type-text))

  (defhydra hydra-leader (:color blue
                                 :hint nil
                                 :idle 0.4)
    "
                                                                      ╭────────┐
   Toggle                        Do                                   │ Leader │
╭─────────────────────────────────────────────────────────────────────┴────────╯
  [_c_] comment                  [_a_] align with regex
  [_f_] fill column              [_p_] show character code
  [_h_] hidden chars             [_i_] insert unicode character (helm)
  [_e_] trailing whitespace      [_<SPC>_] remove trailing whitespaces
  [_v_] font space               [_u_] undo tree
   ^ ^                           [_j_] jump word
   ^ ^                           [_x_] comment box
   ^ ^                           [_r_] expand region
   ^ ^                           [_m_] iedit (multiple edit)
   ^ ^                           [_g_] google translate
   ^ ^                           [_s_] swiper
   ^ ^                           [_t_] helm-semantic-or-imenu
--------------------------------------------------------------------------------
      "
    ("<escape>" nil "quit")
    ("a" align-regexp)
    ("c" evilnc-comment-or-uncomment-lines)
    ("r" er/expand-region)
    ("f" fci-mode)
    ("g" google-translate-smooth-translate)
    ("h" whitespace-mode)
    ("i" helm-ucs)
    ("j" avy-goto-word-1)
    ("m" iedit-mode)
    ("n" count-words)
    ("p" describe-char)
    ("e" joe-toggle-show-trailing-whitespace)
    ("u" undo-tree-visualize)
    ("v" variable-pitch-mode)
    ("<SPC>" whitespace-cleanup)
    ("s" joe-swiper)
    ("t" helm-semantic-or-imenu)
    ("x" comment-box)))

;; markdown

(global-set-key [f9] 'dh-hydra-markdown-mode/body)

(defhydra dh-hydra-markdown-mode (:hint nil)
  "
Formatting        C-c C-s    _s_: bold          _e_: italic     _b_: blockquote   _p_: pre-formatted    _c_: code
Headings          C-c C-t    _h_: automatic     _1_: h1         _2_: h2           _3_: h3               _4_: h4
Lists             C-c C-x    _m_: insert item
Demote/Promote    C-c C-x    _l_: promote       _r_: demote     _u_: move up      _d_: move down
Links, footnotes  C-c C-a    _L_: link          _U_: uri        _F_: footnote     _W_: wiki-link        _R_: reference
"

  ("s" markdown-insert-bold)
  ("e" markdown-insert-italic)
  ("b" markdown-insert-blockquote :color blue)
  ("p" markdown-insert-pre :color blue)
  ("c" markdown-insert-code)

  ("h" markdown-insert-header-dwim)
  ("1" markdown-insert-header-atx-1)
  ("2" markdown-insert-header-atx-2)
  ("3" markdown-insert-header-atx-3)
  ("4" markdown-insert-header-atx-4)

  ("m" markdown-insert-list-item)

  ("l" markdown-promote)
  ("r" markdown-demote)
  ("d" markdown-move-down)
  ("u" markdown-move-up)

  ("L" markdown-insert-link :color blue)
  ("U" markdown-insert-uri :color blue)
  ("F" markdown-insert-footnote :color blue)
  ("W" markdown-insert-wiki-link :color blue)
  ("R" markdown-insert-reference-link-dwim :color blue))

;; moving

(global-set-key (kbd "C-n") 'hydra-move/body)

(defhydra hydra-move (:body-pre (next-line))
  "
   _g_: beginning of buffer  _n_: next line          _f_: forward char   _v_: scroll up
   _G_: end of buffer        _p_: previous line      _b_: backward char  _V_: scroll down
   ^ ^                       _a_: beginning of line  ^ ^                 _l_: top bottom
   ^ ^                       _e_: end of line        ^ ^                 ^ ^
  "
  ("g" beginning-of-buffer)
  ("G" end-of-buffer)
  ("n" next-line)
  ("p" previous-line)
  ("a" beginning-of-line)
  ("e" move-end-of-line)
  ("f" forward-char)
  ("b" backward-char)
  ("v" scroll-up-command)
  ("V" scroll-down-command)
  ("l" recenter-top-bottom))

;; org templates

(define-key org-mode-map "<" (lambda () (interactive)
                               (if (looking-back "^")
                                   (hydra-org-template/body)
                                 (self-insert-command 1))))

(defhydra hydra-org-template (:color blue :hint nil)
  "
_c_enter  _q_uote     _e_macs-lisp    _L_aTeX:
_l_atex   _E_xample   _p_erl          _i_ndex:
_a_scii   _v_erse     _P_erl tangled  _I_NCLUDE:
_s_rc     ^ ^         plant_u_ml      _H_TML:
_h_tml    ^ ^         ^ ^             _A_SCII:
"
  ("s" (hot-expand "<s"))
  ("E" (hot-expand "<e"))
  ("q" (hot-expand "<q"))
  ("v" (hot-expand "<v"))
  ("c" (hot-expand "<c"))
  ("l" (hot-expand "<l"))
  ("h" (hot-expand "<h"))
  ("a" (hot-expand "<a"))
  ("L" (hot-expand "<L"))
  ("i" (hot-expand "<i"))
  ("e" (progn
         (hot-expand "<s")
         (insert "emacs-lisp")
         (forward-line)))
  ("p" (progn
         (hot-expand "<s")
         (insert "perl")
         (forward-line)))
  ("u" (progn
         (hot-expand "<s")
         (insert "plantuml :file CHANGE.png")
         (forward-line)))
  ("P" (progn
         (insert "#+HEADERS: :results output :exports both :shebang \"#!/usr/bin/env perl\"\n")
         (hot-expand "<s")
         (insert "perl")
         (forward-line)))
  ("I" (hot-expand "<I"))
  ("H" (hot-expand "<H"))
  ("A" (hot-expand "<A"))
  ("<" self-insert-command "ins")
  ("o" nil "quit"))

(defun hot-expand (str)
  "Expand org template."
  (insert str)
  (org-try-structure-completion))

;; windows

(global-set-key (kbd "C-M-o") 'hydra-window/body)

(defhydra hydra-window (:color red :columns nil)
  "window"
  ("h" windmove-left nil)
  ("j" windmove-down nil)
  ("k" windmove-up nil)
  ("l" windmove-right nil)
  ("H" hydra-move-splitter-left nil)
  ("J" hydra-move-splitter-down nil)
  ("K" hydra-move-splitter-up nil)
  ("L" hydra-move-splitter-right nil)
  ("v" (lambda ()
         (interactive)
         (split-window-right)
         (windmove-right))
   "vert")
  ("x" (lambda ()
         (interactive)
         (split-window-below)
         (windmove-down))
   "horz")
  ("t" transpose-frame "'")
  ("o" delete-other-windows "one" :exit t)
  ("a" ace-window "ace")
  ("s" ace-swap-window "swap")
  ("d" ace-delete-window "del")
  ("i" ace-maximize-window "ace-one" :exit t)
  ("b" ido-switch-buffer "buf")
  ("m" headlong-bookmark-jump "bmk")
  ("q" nil "cancel")
  ("u" (progn (winner-undo) (setq this-command 'winner-undo)) "undo")
  ("f" nil))

;; zoom

(defhydra hydra-zoom (global-map "<f2>")
  "zoom"
  ("g" text-scale-increase "in")
  ("l" text-scale-decrease "out")
  ("r" (text-scale-set 0) "reset")
  ("0" (text-scale-set 0) :bind nil :exit t)
  ("1" (text-scale-set 0) nil :bind nil :exit t))

;; launcher

(defhydra hydra-launcher (:color blue :columns 2)
  "Launch"
  ("h" man "man")
  ("r" (browse-url "http://www.reddit.com/r/emacs/") "reddit")
  ("w" (browse-url "http://www.emacswiki.org/") "emacswiki")
  ("s" shell "shell")
  ("q" nil "cancel"))

;; rectangle

;; Rectangle edit mode is one of the unique features of Emacs (and Vim) and this
;; hydra makes it easier to interact with it.

(global-set-key (kbd "C-x SPC") 'hydra-rectangle/body)

(defhydra hydra-rectangle (:pre (rectangle-mark-mode 1)
                                :color pink
                                :hint nil)
  "
   _p_: paste   _r_: replace  _I_: insert  _q_: quit
   _y_: copy    _o_: open     _V_: reset
   _d_: kill    _n_: number   _u_: undo
  "
  ("h" backward-char nil)
  ("l" forward-char nil)
  ("k" previous-line nil)
  ("j" next-line nil)
  ("y" copy-rectangle-as-kill)
  ("d" kill-rectangle)
  ("x" clear-rectangle)
  ("o" open-rectangle)
  ("p" yank-rectangle)
  ("r" string-rectangle)
  ("n" rectangle-number-lines)
  ("I" string-insert-rectangle)
  ("u" undo nil)
  ("V" (if (region-active-p)
           (deactivate-mark)
         (rectangle-mark-mode 1)) nil)
  ("q" keyboard-quit :color blue))

;; misc

(global-set-key (kbd "C-M-k") 'hydra-pause-resume)

;; ivy

(use-package swiper
  :demand t
  :bind (("C-c C-r"  . ivy-resume)
         ("C-s"      . swiper)
         :map ivy-minibuffer-map
         ("<return>" . ivy-alt-done)
         ("C-M-h"    . ivy-previous-line-and-call)
         ("C-:"      . ivy-dired)
         ("C-c o"    . ivy-occur)
         :map read-expression-map
         ("C-r"      . counsel-expression-history)
         )
  :config
  (ivy-mode 1)
  (validate-setq ivy-use-virtual-buffers t         ; list `recentf' and bookmarks as well
                 ivy-height 10
                 ivy-count-format "(%d/%d) "       ; counter
                 ivy-extra-directories nil         ; Do not show "./" and "../"
                 ivy-virtual-abbreviate 'full      ; Show full file path
                 ivy-re-builders-alist '((t . ivy--regex-plus))
                 ivy-use-ignore-default 'always    ; ignore buffers set in `ivy-ignore-buffers'
                 ivy-ignore-buffers                ; ignore some buffers in `ivy-switch-buffer'
                 '("company-statistics-cache.el"
                   ".elfeed/index")
                 swiper-action-recenter t          ; always recenter when leaving swiper
                 )

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
  (validate-setq ivy-switch-buffer-faces-alist
                 '((emacs-lisp-mode . swiper-match-face-1)
                   (dired-mode . ivy-subdir)
                   (org-mode . org-level-4)))
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


;; smex order selections accordingly to the most used ones
(use-package smex
  :after swiper
  :config
  (validate-setq smex-save-file (concat
                                 tmp-directory "smex-items")))

;; counsel

(use-package counsel
  :after swiper
  :bind (("M-x"     . counsel-M-x)
         ("C-x C-f" . counsel-find-file)
         ("<f1> f"  . counsel-describe-function)
         ("<f1> v"  . counsel-describe-variable)
         ("<f1> l"  . counsel-load-library)
         ("<f2> i"  . counsel-info-lookup-symbol)
         ("<f2> u"  . counsel-unicode-char)
         ("C-r"     . counsel-grep-or-swiper)
         ("C-c g"   . counsel-git)
         ("C-c j"   . counsel-git-grep)
         ("C-c k"   . counsel-ag)
         ("C-x l"   . counsel-locate)
         ("C-c r"   . counsel-linux-app)
         ("C-x i"   . counsel-imenu)
         ("M-y"     . counsel-yank-pop))
  :config
  (validate-setq  counsel-mode-override-describe-bindings t
                  counsel-find-file-at-point t
                  counsel-find-file-ignore-regexp
                  (concat
                   "\\(?:\\`[#.]\\)"              ; file names beginning with # or .
                   "\\|\\(?:\\`.+?[#~]\\'\\)"     ; file names ending with # or ~
                   )))

;; hydra-ivy replacement

;; A Hydra for ivy/swiper. It helps on ~dired~ also. Check details [[http://oremacs.com/2015/03/26/hydra-ivy-swiper/][here]].

(define-key ivy-minibuffer-map (kbd "C-o") 'hydra-ivy/body)

(defhydra hydra-ivy (:color pink
                            :hint nil)
  "
                                                                      ╭────────┐
     Navigate      Actions         Dired         Quit                 │  Ivy   │
╭─────────────────────────────────────────────────────────────────────┴────────╯
  ^ ^ _k_ ^ ^     [_._] repeat    [_m_] mark    [_i_] cancel
  _h_ ^✜^ _l_     [_r_] reeplace  [_,_] unmark  [_o_] quit
  ^ ^ _j_ ^ ^     [_u_] undo
  "
  ;; arrows
  ("h" ivy-beginning-of-buffer)
  ("j" ivy-next-line)
  ("k" ivy-previous-line)
  ("l" ivy-end-of-buffer)
  ;; actions
  ("." hydra-repeat)
  ("r" ivy-replace)
  ("u" ivy-undo)
  ;; dired
  ("m" ivy-dired-mark)
  ("," ivy-dired-unmark)
  ;; exit
  ("o" keyboard-escape-quit :exit t)
  ("i" nil))

;; Auxiliary functions called by the keys set above
(defun ivy-dired-mark (arg)
  (interactive "p")
  (dotimes (_i arg)
    (with-ivy-window
      (dired-mark 1))
    (ivy-next-line 1)
    (ivy--exhibit)))

(defun ivy-dired-unmark (arg)
  (interactive "p")
  (dotimes (_i arg)
    (with-ivy-window
      (dired-unmark 1))
    (ivy-next-line 1)
    (ivy--exhibit)))

(defun ivy-replace ()
  (interactive)
  (let ((from (with-ivy-window
                (move-beginning-of-line nil)
                (when (re-search-forward
                       (ivy--regex ivy-text) (line-end-position) t)
                  (match-string 0)))))
    (if (null from)
        (user-error "No match")
      (let ((rep (read-string (format "Replace [%s] with: " from))))
        (with-selected-window swiper--window
          (undo-boundary)
          (replace-match rep t t))))))

(defun ivy-undo ()
  (interactive)
  (with-ivy-window
    (undo)))

;; projectile

;; [[https://github.com/bbatsov/projectile][Projectile]] is a project interaction library for Emacs. Its goal is to provide a
;; nice set of features operating on a project level without introducing external
;; dependencies (when feasible). For instance - finding project files has a
;; portable implementation written in pure Emacs Lisp without the use of GNU find
;; (but for performance sake an indexing mechanism backed by external commands
;; exists as well).

;; [[https://github.com/nex3/perspective-el][Perspective]] provides tagged workspaces in Emacs, similar to workspaces in
;; windows managers such as Awesome and XMonad (and somewhat similar to multiple
;; desktops in Gnome or Spaces in OS X).

;; Commands are all prefixed by ~C-x x~

;; | Key        | Command             | What it does                                                    |
;; |------------+---------------------+-----------------------------------------------------------------|
;; | s          | persp-switch        | Query a perspective to switch or create                         |
;; | k          | persp-remove-buffer | Query a buffer to remove from current perspective               |
;; | c          | persp-kill          | Query a perspective to kill                                     |
;; | r          | persp-rename        | Rename current perspective                                      |
;; | a          | persp-add-buffer    | Query an open buffer to add to current perspective              |
;; | A          | persp-set-buffer    | Add buffer to current perspective and remove it from all others |
;; | i          | persp-import        | Import a given perspective from another frame.                  |
;; | n, <right> | persp-next          | Switch to next perspective                                      |
;; | p, <left>  | persp-prev          | Switch to previous perspective                                  |

;;(use-package projectile
;;  :ensure t
;;  :config
;;  (projectile-global-mode)
  ;;;;(setq magit-completing-read-function 'ivy-completing-read)
;;  (setq projectile-completion-system 'ivy))

(use-package projectile
  :config
  (validate-setq projectile-cache-file (concat
                                        tmp-directory "projectile.cache")
                 projectile-known-projects-file (concat
                                                 tmp-directory "projectile-bookmarks.eld")
                 projectile-enable-caching t
                 projectile-completion-system 'ivy
                 projectile-switch-project-action 'projectile-dired
                 projectile-mode-line '(:eval (format " :%s:" (projectile-project-name))))
  (projectile-global-mode))

(use-package perspective
  :config
  (persp-mode))

(use-package persp-projectile
  :config
  (define-key projectile-mode-map (kbd "s-s")
    'projectile-persp-switch-project))

(use-package counsel-projectile
  :config
  (counsel-projectile-on))

(use-package ibuffer-projectile)

;; magit

(use-package magit
  :config
  (setq magit-completing-read-function 'ivy-completing-read
        magit-display-buffer-function 'magit-display-buffer-fullframe-status-topleft-v1))

;; ace-window

(use-package ace-window)

;; ag

;; [[https://github.com/Wilfred/ag.el][Ag.el]] allows you to search using [[https://github.com/ggreer/the_silver_searcher][ag (The Silver Searcher)]] from inside Emacs. You
;; can filter by file type, edit results inline, or find files.

(use-package ag
  :config
  (validate-setq ag-highlight-search t))

;; anzu
;; anzu provides a minor mode which displays current match and total matches
;; information in the mode-line in various search modes.

(use-package anzu
  :bind (("M-%" . anzu-query-replace)
         ("C-M-%" . anzu-query-replace-regexp))
  :config
  (global-anzu-mode +1))

;; async

;; Simple library for asynchronous processing in Emacs

(use-package async
  :config
  (dired-async-mode t)
  (async-bytecomp-package-mode t))

;; avy
;; avy is a GNU Emacs package for jumping to visible text using a char-based
;; decision tree. See also ace-jump-mode and vim-easymotion - avy uses the same
;; idea.

(use-package avy
  :bind (("C-:" . avy-goto-char)
         ("C-'" . avy-goto-char-2)
         ("M-g f" . avy-goto-line)
         ("M-g w" . avy-goto-word-1)
         ("M-g e" . avy-goto-word-0))
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

;; beacon
;; Never lose your cursor again. Whenever the window scrolls a light will shine on
;; top of your cursor so you know where it is.

(use-package beacon
  :demand t
  :config
  (beacon-mode t)
  (setq beacon-push-mark 35
        beacon-color "#666600"))

;; bookmarks
;; Bookmarks to files and directories

(use-package bookmark
  :config
  (setq bookmark-completion-ignore-case nil)
  (bookmark-maybe-load-default-file))

;; company

;; Company is a text completion framework for Emacs. The name stands for “complete
;; anything”. It uses pluggable back-ends and front-ends to retrieve and display
;; completion candidates.

;; It comes with several back-ends such as Elisp, Clang, Semantic, Eclim, Ropemacs,
;; Ispell, CMake, BBDB, Yasnippet, dabbrev, etags, gtags, files, keywords and a few
;; others.

;; The CAPF back-end provides a bridge to the standard
;; completion-at-point-functions facility, and thus works with any major mode that
;; defines a proper completion function.

(use-package company
  :bind (("C-c /" . company-files))                      ; force complete file names on "C-c /" key
  :config
    (add-hook 'after-init-hook 'global-company-mode)
    (setq company-tooltip-limit 20                       ; bigger popup window
          company-tooltip-align-annotations 't           ; align annotations to the right tooltip border
          company-idle-delay .3                          ; decrease delay before autocompletion popup shows
          company-begin-commands '(self-insert-command)) ; start autocompletion only after typing
    )

(use-package company-statistics
  :after company
  :config
  (setq company-statistics-file
        (concat tmp-directory "company-statistics-cache.el"))

  (add-hook 'after-init-hook 'company-statistics-mode))

;; (use-package helm-company
;;   :ensure t
;;   :config
;;   (eval-after-load 'company
;;     '(progn
;;        (define-key company-mode-map (kbd "C-:") 'helm-company)
;;        (define-key company-active-map (kbd "C-:") 'helm-company))))

(use-package slime-company
  :after company
  :config
  (slime-setup '(slime-fancy slime-company)))

(use-package company-ansible
  :after company
  :config
  (add-to-list 'company-backends 'company-ansible))

(use-package company-math
  :after company
  :config
    (add-to-list 'company-backends '((company-math-symbols-unicode)
                                     (company-math-symbols-latex)
                                     (company-latex-commands)))
    (setq company-tooltip-align-annotations t))

;; crux

;; [[https://github.com/bbatsov/crux][crux]] is a Collection of Ridiculously Useful eXtensions for Emacs. crux bundles a
;; few useful interactive commands to enhance your overall Emacs experience.

(use-package crux)

;; diff-hl

;; [[https://github.com/dgutov/diff-hl][diff-hl-mode]] highlights uncommitted changes on the left side of the window,
;; allows you to jump between and revert them selectively.

;; *Keybindings*

;; | *function*             | *Keybinding* |
;; |------------------------+--------------|
;; | diff-hl-diff-goto-hunk | C-x v =      |
;; | diff-hl-revert-hunk    | C-x v n      |
;; | diff-hl-previous-hunk  | C-x v [      |
;; | diff-hl-next-hunk      | C-x v ]      |

(use-package diff-hl
  :demand t
  :config
  (global-diff-hl-mode)
  (diff-hl-flydiff-mode)
  (add-hook 'dired-mode-hook 'diff-hl-dired-mode)
  (add-hook 'magit-post-refresh-hook 'diff-hl-magit-post-refresh))

;; dired

;; Load up the assorted dired extensions.

(use-package dired-x
  :ensure nil)

(use-package dired+
  :config
  (setq diredp-hide-details-initially-flag t
        diredp-hide-details-propagate-flag t)

  (toggle-diredp-find-file-reuse-dir 1)        ; use single buffer for all dired navigation
  ;;(diredp-make-find-file-keys-reuse-dirs)
  )

(use-package dired-open)

;; Dired configuration
(setq dired-clean-up-buffers-too t      ; kill buffer of files/dir that are deleted in dired
      dired-recursive-copies 'always    ; always copy directories recursively
      dired-recursive-deletes 'top      ; ask before recursively deleting a directory
      dired-open-extensions             ; open files with appropriate programs
      '(("pdf" . "evince")
        ("mkv" . "vlc")
        ("mp4" . "vlc")
        ("avi" . "vlc")))

(setq-default dired-listing-switches "-lhvA")  ; ls switches when dired gets list of files

;; easy-kill

;; [[https://github.com/leoliu/easy-kill][Provide commands]] ~easy-kill~ and ~easy-mark~ to let users kill or mark things
;; easily.

;; | *Key* | *Command* | *Action*                                   |
;; |-------+-----------+--------------------------------------------|
;; | M-w w |           | save word at point                         |
;; | M-w s |           | save sexp at point                         |
;; | M-w l |           | save list at point (enclosing sexp)        |
;; | M-w d |           | save defun at point                        |
;; | M-w D |           | save current defun name                    |
;; | M-w f |           | save file at point                         |
;; | M-w b |           | save buffer-file-name or default-directory |
;; |       |           |                                            |

;; The following keys modify the selection:

;; | *Key* | *Command* | *Action*                                                                                                    |
;; |-------+-----------+-------------------------------------------------------------------------------------------------------------|
;; | @     |           | append selection to previous kill and exit. For example, M-w d @ will append current function to last kill. |
;; | C-w   |           | kill selection and exit                                                                                     |
;; | +, -  |           | and 1..9: expand/shrink selection                                                                           |
;; | 0     |           | shrink the selection to the initial size i.e. before any expansion                                          |
;; | C-SPC |           | turn selection into an active region                                                                        |
;; | C-g   |           | abort                                                                                                       |
;; | ?     |           | help                                                                                                        |

(use-package easy-kill
  :bind (([remap kill-ring-save] . easy-kill)
         ([remap mark-sexp] . easy-mark)))

;; ediff
;; The default ~ediff-mode~ isn't quite optimized. The following settings are taken
;; from [[http://oremacs.com/2015/01/17/setting-up-ediff/][Oremacs]].

;; Just a note about the ~--text~ in the ~ediff-diff-options~: it will force the
;; GNU utility ~diff~, which is called by ~ediff~, to treat the input files as text
;; files. This is necessary as the utility ~diff~ doesn't understand unicode, and
;; sees unicode encoded files as binary files ([[http://stackoverflow.com/questions/10503937/emacs-ediff-foreign-character-sets-and-text-file-encodings][stackoverflow]]).

(use-package ediff
  :ensure nil
  :config
  (csetq ediff-window-setup-function 'ediff-setup-windows-plain)
  (csetq ediff-split-window-function 'split-window-horizontally)
  (csetq ediff-diff-options "-w --text")

  (defun ora-ediff-prepare-buffer ()
    (when (memq major-mode '(org-mode emacs-lisp-mode))
      (outline-show-all)))

  (add-hook 'ediff-prepare-buffer-hook 'ora-ediff-prepare-buffer)

  (defun ora-ediff-jk ()
    (define-key ediff-mode-map "j" 'ediff-next-difference)
    (define-key ediff-mode-map "k" 'ediff-previous-difference))

  (add-hook 'ediff-keymap-setup-hook #'ora-ediff-jk)

;;;###autoload
  (defun ora-ediff-hook ())

;;;###autoload
  (defun ora-diff-hook ())

  (mapc
   (lambda (k)
     (define-key diff-mode-map k
       `(lambda () (interactive)
          (if (region-active-p)
              (replace-regexp "^." ,k nil
                              (region-beginning)
                              (region-end))
            (insert ,k)))))
   (list " " "-" "+"))
  )

;; expand-region

;; [[https://github.com/magnars/expand-region.el][Expand region]] increases the selected region by semantic units. Just keep
;; pressing the key until it selects what you want.

(use-package expand-region)

;; fill-column-indicator
;; Toggle the vertical column that indicates the fill threshold.

(use-package fill-column-indicator
  :config
  (fci-mode t)
  (setq fci-rule-width 1
        fci-rule-color "#5d478b"
        fci-rule-column 80))

;; fixmee

;; Fixmee-mode tracks fixme notices in code comments, highlights them, ranks them
;; by urgency, and lets you navigate to them quickly.

;; It requires [[https://github.com/rolandwalker/button-lock][button-lock.el]], which is installed by ~wiki-nav~.

;; *Patterns* - The following fixme patterns are supported by default:

;; @@@
;; XXX         ; only this one is case-sensitive
;; todo
;; fixme

;; *Key bindings*

;; | Keystrokes | 	Function                                             |
;; |------------+----------------------------------------------------------|
;; | C-c f      | fixmee-goto-nextmost-urgent                              |
;; | C-c F      | fixmee-goto-prevmost-urgent                              |
;; | C-c v      | fixmee-view-listing                                      |
;; | M-n        | fixmee-goto-next-by-position ; only when the point is    |
;; | M-p        | fixmee-goto-previous-by-position ; inside a fixme notice |

(use-package wiki-nav
  :config
  (global-wiki-nav-mode 1))

(use-package fixmee
  :after wiki-nav
  :config
  (global-fixmee-mode 1))

;; flx
;;  Fuzzy matching for Emacs ... a la Sublime Text. It is needed for fuzzy matching
;;  in swiper + avy.

(use-package flx)

;; flycheck

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

;; Linting prose

;; I use [[http://proselint.com/][proselint]] to check my prose for common errors. This creates a flycheck
;; checker that runs proselint in texty buffers and displays my errors.

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

;; graphviz-dot-mode

;; [[https://github.com/ppareit/graphviz-dot-mode][graphviz-dot-mode]] is a mode for the DOT language, used by =graphviz=.

(use-package graphviz-dot-mode)

;; neotree

(use-package neotree
  :bind (("<f6>" . neotree-toggle))
  :config
  (validate-setq neo-theme (if window-system 'icons 'arrow)))

;; paradox
;; Project for modernizing Emacs’ Package Menu. With package ratings, usage
;; statistics, customizability, and more.

(use-package paradox
  :config
  (setq paradox-github-token t
        paradox-automatically-star nil
        paradox-execute-asynchronously t))

;; pass

(use-package pass
  :ensure t)

;; pcache

;; [[https://github.com/sigma/pcache][pcache]] provides a persistent way of caching data, in a hashtable-like structure.
;; It relies on `eieio-persistent' in the backend, so that any object that can be
;; serialized by EIEIO can be stored with pcache.

;; [[https://github.com/rolandwalker/persistent-soft][persistent-soft]] is a wrapper around pcache.el, providing "soft" fetch and store routines
;; which never throw an error, but instead return nil on failure.

(use-package pcache
  :demand t)

(use-package persistent-soft
  :demand t
  :after pcache)

;; recentf

;; Recentf is a minor mode that builds a list of recently opened files. This list
;; is automatically saved across Emacs sessions. You can access the list through a
;; menu. Here it's set to work together with ivy-switch-buffer.

;; [[https://github.com/abo-abo/swiper/releases][source 1]] - [[https://www.masteringemacs.org/article/find-files-faster-recent-files-package][source 2]] - [[http://emacsredux.com/blog/2013/04/05/recently-visited-files/][source 3]]

(use-package recentf
  :config
  (recentf-mode t)
  (validate-setq recentf-save-file (concat
                                    tmp-directory "recentf")
                 recentf-max-saved-items 10
                 recentf-exclude '("COMMIT_MSG"
                                   "COMMIT_EDITMSG"
                                   "github.*txt$"
                                   ".*png$")))

;; uniquify

;; Nicer naming of buffers for files with identical names. [[https://github.com/purcell/emacs.d/blob/master/lisp/init-uniquify.el][source]]

(use-package uniquify
  :ensure nil
  :config
  (setq uniquify-buffer-name-style 'reverse
        uniquify-separator " • "
        uniquify-after-kill-buffer-p t       ; rename after killing uniquified
        uniquify-ignore-buffers-re "^\\*"))  ; don't muck with special buffers

;; undo-tree

;; =C-x u= to ~undo-tree-visualize~
;; C-/ undo
;; S-C-/ redo

(use-package undo-tree
  :init
  (global-undo-tree-mode))

;; volatile-highlights

;; It provides minor mode volatile-highlights-mode, which brings visual feedback
;; to some operations (eg. pasting, etc) by highlighting portions relating to the
;; operations.

(use-package volatile-highlights
  :config
  (volatile-highlights-mode t))

;; window-numbering

;; Numbered window shortcuts for Emacs
;; Enable window-numbering-mode and use M-1 through M-0 to navigate.

;; The defun *window-numbering-install-mode-line* set below is to make
;; window-numbering work together with spaceline, overriding its own modeline
;; display function.

(use-package window-numbering
  :init   (window-numbering-mode)
  :config
  (defun window-numbering-install-mode-line (&optional position)
    "Do nothing."))

;; wgrep

;; [[https://github.com/mhayashi1120/Emacs-wgrep][wgrep]] is a writable grep buffer and apply the changes to files

;; You can edit the text in the grep buffer after typing ~C-c C-p~. After that the
;; changed text is highlighted. The following keybindings are defined:

;; | Key      | Action                                              |
;; |----------+-----------------------------------------------------|
;; | C-c C-e: | Apply the changes to file buffers.                  |
;; | C-c C-u: | All changes are unmarked and ignored.               |
;; | C-c C-d: | Mark as delete to current line (including newline). |
;; | C-c C-r: | Remove the changes in the regiond                   |
;; | C-c C-p: | Toggle read-only area.                              |
;; | C-c C-k: | Discard all changes and exit.                       |
;; | C-x C-q: | Exit wgrep mode.                                    |

(use-package wgrep
  :config
  (progn
    (with-eval-after-load 'grep
      (bind-key "C-x C-q" #'wgrep-change-to-wgrep-mode grep-mode-map))

    (with-eval-after-load 'wgrep
      (bind-key "C-c C-c" #'wgrep-finish-edit grep-mode-map))))

;; which-key
;; Displays the key bindings following your currently entered incomplete command (a
;; prefix) in a popup.

(use-package which-key
  :init (which-key-mode)
  :config (setq which-key-idle-delay 0.5
                which-key-key-replacement-alist
                '(("<\\([[:alnum:]-]+\\)>" . "\\1")
                  ("up"                  . "↑")
                  ("right"               . "→")
                  ("down"                . "↓")
                  ("left"                . "←")
                  ("DEL"                 . "⌫")
                  ("deletechar"          . "⌦")
                  ("RET"                 . "⏎"))))

;; mu4e

(add-to-list 'load-path "/usr/local/share/emacs/site-lisp/mu4e/")
(load "~/my-prj/dotfiles/init-mu4e.el" t)

;; mu4e-contrib

(use-package mu4e-contrib
  :after mu4e
  :ensure nil
  :config
  ;; Custom marks
  (validate-setq mu4e-headers-draft-mark     '("D" . "📝 ") ;; ✒ ✏
                 mu4e-headers-flagged-mark   '("F" . "🏴 ")
                 mu4e-headers-new-mark       '("N" . "✉ ")
                 mu4e-headers-passed-mark    '("P" . "→ ") ;; ↪
                 mu4e-headers-replied-mark   '("R" . "← ")
                 mu4e-headers-seen-mark      '("S" . "🗸")  ;; ✓ 🗸 ✔
                 mu4e-headers-trashed-mark   '("T" . "✗ ") ;; 🗑
                 mu4e-headers-attach-mark    '("a" . "📎 ")
                 mu4e-headers-encrypted-mark '("x" . "🔐 ")
                 mu4e-headers-signed-mark    '("s" . "🔏 ")
                 mu4e-headers-unread-mark    '("u" . "● ") ;; ★
                 mu4e-headers-empty-parent-prefix '("-" . "○")
                 mu4e-headers-first-child-prefix  '("\\" . "┗━❯")
                 mu4e-headers-has-child-prefix    '("+" . "┗◉")
                 mu4e-headers-duplicate-prefix    '("=" . "⚌")
                 mu4e-headers-default-prefix      '("|" . "┃")

                 mu4e-headers-date-format "%Y-%m-%d %H:%M"       ; date format of the header list
                 mu4e-headers-time-format "%H:%M"                ; time format of the header list
                 mu4e-date-format-long "%A %Y-%m-%d %T %z (%Z)"  ; date format in the message view

                 mu4e-headers-fields '((:date           . 20)
                                       (:flags          . 7)
                                       (:mailing-list   . 15)
                                       (:from-or-to     . 40)
                                       (:thread-subject . nil))
                 )

  ;; try to emulate some of the eww key-bindings
  (add-hook 'mu4e-view-mode-hook
            (lambda ()
              (local-set-key (kbd "<tab>") 'shr-next-link)
              (local-set-key (kbd "<backtab>") 'shr-previous-link))))

;; org-mu4e

(use-package org-mu4e
  :after mu4e
  :ensure nil)

;; mu4e-alert

(use-package mu4e-alert
  :after mu4e
  :init
  (add-hook 'after-init-hook
            #'mu4e-alert-enable-notifications)       ; enable notifications
  (add-hook 'after-init-hook
            #'mu4e-alert-enable-mode-line-display)   ; display unread email count in the mode-line
  :config
  (validate-setq
   mu4e-alert-email-notification-types '(count))     ; display only total number of unread emails

  ;; Set notification style accordingly to the OS
  (cond ((eq system-type 'gnu/linux)
         (mu4e-alert-set-default-style 'libnotify)
         ;;(mu4e-alert-set-default-style 'notifications) ; alternative
         )
        ((eq system-type 'darwin)
         (mu4e-alert-set-default-style 'notifier)
         ;; (mu4e-alert-set-default-style 'growl)        ; alternative
         ))

  (alert-add-rule
   :category "mu4e-alert"
   :predicate (lambda (_) (string-match-p "^mu4e-" (symbol-name major-mode)))
   :continue t))

;; mu4e-maildirs-extension

;; It adds a maildir summary in mu4e-main-view.

(use-package mu4e-maildirs-extension
  :after mu4e
  :config
  (mu4e-maildirs-extension)
  (validate-setq mu4e-maildirs-extension-maildir-separator    "*"
                 mu4e-maildirs-extension-submaildir-separator "✉"
                 mu4e-maildirs-extension-action-text          nil))

;; Sending emails asynchronous

;; This is useful to send emails with attachments and do not block emacs until end
;; the transmission. It requires [[https://github.com/jwiegley/emacs-async][emacs-async]].

(use-package smtpmail-async
  :ensure async
  :config
  (validate-setq
   send-mail-function 'async-smtpmail-send-it
   message-send-mail-function 'async-smtpmail-send-it))

;; Decorate mu main view

(defun my-mu4e-main-mode-font-lock-rules ()
  (save-excursion
    (goto-char (point-min))
    (while (re-search-forward "\\[\\([a-zA-Z]\\{1,2\\}\\)\\]" nil t)
      (add-text-properties (match-beginning 1) (match-end 1)
                           '(face font-lock-variable-name-face)))))
(add-hook 'mu4e-main-mode-hook 'my-mu4e-main-mode-font-lock-rules)

;; more cool and practical than the default
(setq mu4e-headers-from-or-to-prefix '("" . "➜ "))

;; Fontify mu faces

;; [[https://groups.google.com/forum/#!topic/mu-discuss/AJ1A-Z8RMv4][source]]

(defgroup mu4e-faces nil
  "Type faces (fonts) used in mu4e."
  :group 'mu4e
  :group 'faces)

(defface mu4e-basic-face
  '((t :inherit font-lock-keyword-face))
  "Basic Face."
  :group 'mu4e-faces)

(defface mu4e-list-default
  '((t :inherit mu4e-basic-face))
  "Basic list Face."
  :group 'mu4e-faces)

(defface mu4e-rw-default
  '((t :inherit mu4e-basic-face))
  "Basic rw Face."
  :group 'mu4e-faces)

;; basic face from where the rest inherits
'(mu4e-basic-face ((t :inherit font-lock-keyword-face :weight normal :foreground "Gray10")))

;; read-write group
'(mu4e-rw-default ((t :inherit mu4e-basic-face))) ;; face from where all the read/write faces inherits
'(mu4e-header-face ((t :inherit mu4e-rw-default)))
'(mu4e-header-marks-face ((t :inherit mu4e-rw-default)))
'(mu4e-header-title-face ((t :inherit mu4e-rw-default)))
'(mu4e-header-highlight-face ((t :inherit mu4e-rw-default :foreground "Black" :background "LightGray")))
'(mu4e-compose-header-face ((t :inherit mu4e-rw-default)))
'(mu4e-compose-separator-face ((t :inherit mu4e-rw-default :foreground "Gray30" :weight bold)))
'(mu4e-footer-face  ((t :inherit mu4e-rw-default)))
'(mu4e-contact-face ((t :inherit mu4e-rw-default   :foreground "Black")))
'(mu4e-cited-1-face ((t :inherit mu4e-rw-default   :foreground "Gray10")))
'(mu4e-cited-2-face ((t :inherit mu4e-cited-1-face :foreground "Gray20")))
'(mu4e-cited-3-face ((t :inherit mu4e-cited-2-face :foreground "Gray30")))
'(mu4e-cited-4-face ((t :inherit mu4e-cited-3-face :foreground "Gray40")))
'(mu4e-cited-5-face ((t :inherit mu4e-cited-4-face :foreground "Gray50")))
'(mu4e-cited-6-face ((t :inherit mu4e-cited-5-face :foreground "Gray60")))
'(mu4e-cited-7-face ((t :inherit mu4e-cited-6-face :foreground "Gray70")))
'(mu4e-link-face    ((t :inherit mu4e-rw-default   :foreground "Blue" :weight bold)))
'(mu4e-system-face  ((t :inherit mu4e-rw-defaul    :foreground "DarkOrchid")))
'(mu4e-url-number-face ((t :inherit mu4e-rw-default :weight bold)))
'(mu4e-attach-number-face ((t :inherit mu4e-rw-default :weight bold :foreground "Blue")))

;; lists (headers) group
'(mu4e-list-default ((t :inherit mu4e-basic-face))) ;; basic list face from where lists inherits
'(mu4e-draft-face   ((t :inherit mu4e-list-default)))
'(mu4e-flagged-face ((t :inherit mu4e-list-default :weight bold :foreground "Black")))
'(mu4e-forwarded-face ((t :inherit mu4e-list-default)))
'(mu4e-list-default-face ((t :inherit mu4e-list-default)))
'(mu4e-title-face    ((t :inherit mu4e-list-default)))
'(mu4e-trashed-face  ((t :inherit mu4e-list-default)))
'(mu4e-warning-face  ((t :inherit mu4e-list-default :foreground "OrangeRed1")))
'(mu4e-modeline-face ((t :inherit mu4e-list-default)))
'(mu4e-moved-face    ((t :inherit mu4e-list-default)))
'(mu4e-ok-face       ((t :inherit mu4e-list-default :foreground "ForestGreen")))
'(mu4e-read-face     ((t :inherit mu4e-list-default :foreground "Gray80")))
'(mu4e-region-code-face ((t :inherit mu4e-list-default :background "Gray25")))
'(mu4e-replied-face   ((t :inherit mu4e-list-default :foreground "Black")))
'(mu4e-unread-face    ((t :inherit mu4e-list-default :foreground "Blue")))
'(mu4e-highlight-face ((t :inherit mu4e-unread-face)))

'(mu4e-special-header-value-face ((t :inherit mu4e-contact-face)))
'(mu4e-header-key-face   ((t :inherit mu4e-contact-face :foreground "Gray50")))
'(mu4e-header-value-face ((t :inherit mu4e-contact-face)))
'(message-cited-text     ((t :inherit mu4e-rw-default :foreground "Gray10")))

;; elfeed

;; Elfeed is an extensible web feed reader for Emacs, supporting both Atom and RSS.
;; Elfeed was inspired by notmuch.

;; =elfeed-org= is used to manage the feeds, enjoying the power of *orgmode* to
;; keep everything tidy and organized :).

;; =elfeed-goodies= give various bits and pieces to enhance the Elfeed user
;; experience.

;; - [[https://github.com/skeeto/elfeed][Source]] and [[http://nullprogram.com/blog/2013/09/04/][info]]
;; - [[http://nullprogram.com/blog/2013/11/26/][Tips & tricks]]
;; - [[http://pragmaticemacs.com/emacs/read-your-rss-feeds-in-emacs-with-elfeed/][Some]] [[http://pragmaticemacs.com/emacs/a-tweak-to-elfeed-filtering/][tweaks]] - this is from where the custom functions were taken (great article)
;; - [[https://github.com/remyhonig/elfeed-org][elfeed-org]]
;; - [[https://github.com/algernon/elfeed-goodies][elfeed-goodies]]

;;   | *Key* | *Function*                                           |
;;   |-------+------------------------------------------------------|
;;   | g     | refresh view of the feed listing                     |
;;   | G     | fetch feed updates from the servers                  |
;;   | s     | update the search filter (see tags)                  |
;;   | RET   | view selected entry in a buffer                      |
;;   | b     | open selected entries in your browser (`browse-url`) |
;;   | y     | copy selected entries URL to the clipboard           |
;;   | r     | mark selected entries as read                        |
;;   | u     | mark selected entries as unread                      |
;;   | +     | add a specific tag to selected entries               |
;;   | -     | remove a specific tag from selected entries          |

(use-package elfeed
  :commands elfeed
  :bind (("C-x w" . bjm/elfeed-load-db-and-open)
         :map elfeed-search-mode-map
         ("\\" . hydra-elfeed-search/body)
         ("q"  . bjm/elfeed-save-db-and-bury)
         :map elfeed-show-mode-map
         ("\\"    . hydra-elfeed-show/body)
         ("j"     . elfeed-show-next)
         ("k"     . elfeed-show-prev)
         ("o"     . elfeed-show-visit)
         ("<escape>" . keyboard-quit)
         ("SPC"   . scroll-up)
         ("S-SPC" . scroll-down)
         ("TAB"   . shr-next-link)
         ("S-TAB" . shr-previous-link)
         ))

;; load list of feeds from an org file \o/
(use-package elfeed-org
  :after elfeed
  :config
  (elfeed-org)
  (setq rmh-elfeed-org-files (list "~/org/elfeed.org")))

;; other special goodies for elfeed
(use-package elfeed-goodies
  :after elfeed
  :config
  (elfeed-goodies/setup))

;; functions to support syncing ~/.elfeed between machines (I'm using syncthing)
;; makes sure elfeed reads index from disk before launching
(defun bjm/elfeed-load-db-and-open ()
  "Wrapper to load the elfeed db from disk before opening"
  (interactive)
  (elfeed)
  (elfeed-db-load)
  (elfeed-search-update--force))

;; write to disk when quiting
(defun bjm/elfeed-save-db-and-bury ()
  "Wrapper to save the elfeed db to disk before burying buffer"
  (interactive)
  (elfeed-db-save)
  (quit-window))

;; Hydra
(defhydra hydra-elfeed-search (:color pink
                                      :hint nil)
  "
                                                                                ╭────────┐
            Move   Filter     Entries        Tags          Do                   │ Elfeed │
          ╭─────────────────────────────────────────────────────────────────────┴────────╯
            _p_/_k_    [_s_] live   [_RET_] view     [_r_] read      [_a_] refresh
            ^ ^↑^ ^    [_S_] set    [_o_] browse     [_u_] unread    [_A_] fetch
            ^ ^ ^ ^    [_e_] emacs  [_y_] yank url   [_+_] add       [_d_] unjam
            ^ ^↓^ ^    [_l_] linux  [_v_] mark       [_-_] remove    [_E_] edit feeds
            _n_/_j_     ^ ^          ^ ^              ^ ^            [_q_] exit
          --------------------------------------------------------------------------------
                  "
  ("q"    bjm/elfeed-save-db-and-bury :color blue)
  ("a"    elfeed-search-update--force)
  ("A"    elfeed-update)
  ("d"    elfeed-unjam)
  ("s"    elfeed-search-live-filter)
  ("S"    elfeed-search-set-filter)
  ("e"    (elfeed-search-set-filter "+emacs"))
  ("l"    (elfeed-search-set-filter "+linux"))
  ("RET"  elfeed-search-show-entry :color blue)
  ("o"    elfeed-search-browse-url)
  ("y"    elfeed-search-yank)
  ("v"    set-mark-command)
  ("n"    next-line)
  ("j"    next-line)
  ("p"    previous-line)
  ("k"    previous-line)
  ("r"    elfeed-search-untag-all-unread)
  ("u"    elfeed-search-tag-all-unread)
  ("E"    (lambda() (interactive)(find-file "~/org/elfeed.org")))
  ("+"    elfeed-search-tag-all)
  ("-"    elfeed-search-untag-all))

(defhydra hydra-elfeed-show (:color pink
                                    :hint nil)
  "
                                                                                ╭────────┐
            Scroll       Entries        Tags          Links                     │ Elfeed │
          ╭─────────────────────────────────────────────────────────────────────┴────────╯
            _S-SPC_    _p_/_k_  [_g_] refresh   [_u_] unread    _S-TAB_
            ^  ↑  ^    ^ ^↑^ ^  [_o_] browse    [_+_] add       ^  ↑  ^
            ^     ^    ^ ^ ^ ^  [_y_] yank url  [_-_] remove    ^     ^
            ^  ↓  ^    ^ ^↓^ ^  [_q_] quit       ^ ^            ^  ↓  ^
             _SPC_     _n_/_j_  [_s_] quit & search^^            _TAB_
          --------------------------------------------------------------------------------
                  "
  ("<ESC>" nil "quit")
  ("q"     elfeed-kill-buffer :color blue)
  ("g"     elfeed-show-refresh)
  ("n"     elfeed-show-next)
  ("j"     elfeed-show-next)
  ("p"     elfeed-show-prev)
  ("k"     elfeed-show-prev)
  ("s"     elfeed-show-new-live-search)
  ("o"     elfeed-show-visit)
  ("y"     elfeed-show-yank)
  ("u"     (elfeed-show-tag 'unread))
  ("+"     elfeed-show-tag)
  ("-"     elfeed-show-untag)
  ("SPC"   scroll-up)
  ("S-SPC" scroll-down)
  ("TAB"   shr-next-link)
  ("S-TAB" shr-previous-link))

;; engine-mode

;; [[https://github.com/hrs/engine-mode][engine-mode]] is a global minor mode for Emacs. It enables you to easily define
;; search engines, bind them to keybindings, and query them from the comfort of
;; your editor.

;; Just select the text you want to be searched on the internet and press =C-c /=
;; (default), then choose the engine.

(use-package engine-mode
  :config
  (defengine duckduckgo
    "https://duckduckgo.com/?q=%s"
    :keybinding "d")

  (defengine github
    "https://github.com/search?ref=simplesearch&q=%s"
    :keybinding "g")

  (defengine google
    "http://www.google.com/search?ie=utf-8&oe=utf-8&q=%s")

  (defengine rfcs
    "http://pretty-rfc.herokuapp.com/search?q=%s")

  (defengine stack-overflow
    "https://stackoverflow.com/search?q=%s"
    :keybinding "s")

  (defengine wikipedia
    "http://www.wikipedia.org/search-redirect.php?language=en&go=Go&search=%s"
    :keybinding "w")

  (defengine wiktionary
    "https://www.wikipedia.org/search-redirect.php?family=wiktionary&language=en&go=Go&search=%s")

  (engine-mode t))

;; hunspell setup

;; 1. Install Hunspell from [[http://hunspell.sourceforge.net/][Hunspell page]] or use your distribution package manager
;; 2. Download the language dictionary extension from [[http://extensions.libreoffice.org/extension-center][Libreoffice]] or  [[http://extensions.openoffice.org/en/project/english-dictionaries-apache-openoffice][Openoffice]]
;; 3. It will download the file ~<language>.oxt~. Rename it to ~<language>.zip~ and unzip
;;    it into a temporary folder.
;; 4. Copy the ~<language>.dic~ and ~<language>.aff~ files from there to a folder where you save
;;    dictionary files, usually to =~/usr/local/share/hunspell/= or =~/usr/share/hunspell/=
;; 5. Add that path to shell env variable ~DICPATH~: =setenv DICPATH $MYLOCAL/share/hunspell=
;; 6. Restart emacs so that when hunspell is run by ispell/flyspell, that env variable is effective.

;; Hunspell will search for a dictionary called ~en_US~ in the path specified by =$DICPATH=.

(use-package flyspell
  :demand t
  :config
  (progn
    (validate-setq ispell-program-name "hunspell"
                   ispell-dictionary "en_GB"
                   ispell-dictionary-alist '(("en_GB" "[[:alpha:]]" "[^[:alpha:]]" "[']" nil ("-d" "en_GB") nil utf-8)))
    ;;(add-to-list 'ispell-dictionary-alist '("it_IT" "[[:alpha:]]" "[^[:alpha:]]" "[']" nil ("-d" "it_IT") nil utf-8))
    )

  (validate-setq flyspell-issue-welcome-flag nil      ; turn off flyspell welcome message
                 flyspell-issue-message-flag nil)     ; turn off flyspell messages when checking words

  (add-hook 'prog-mode-hook     'flyspell-prog-mode)  ; spell check in program comments
  (add-hook 'org-mode-hook      'flyspell-mode)       ; spell check in md/plain text/org-mode
  (add-hook 'text-mode-hook     'flyspell-mode)
  (add-hook 'markdown-mode-hook 'flyspell-mode))

;; Switch dictionaries

;; Switch between the most used dictionaries in my case.

(defun rts-switch-dictionary () (interactive)
       (let* ((dic ispell-current-dictionary)
              (change (if (string= dic "en_GB") "it_IT" "en_GB")))
         (ispell-change-dictionary change)
         (message "Dictionary switched from %s to %s" dic change)))

(global-set-key (kbd "<f8>") 'rts-switch-dictionary)

;; flyspell-lazy

;; Flyspell usually slows down the responsiveness when writing texts. [[https://github.com/rolandwalker/flyspell-lazy][flyspell-lazy]]
;; is used to improve *Flyspell* responsiveness using idle timers.

(use-package flyspell-lazy
  :demand t
  :after flyspell
  :config
  (flyspell-lazy-mode 1))

;; flyspell-popup

;; [[https://github.com/xuchunyang/flyspell-popup][Flyspell-popup]] is used to correct words with Flyspell in popup menus.

(use-package flyspell-popup
  :after flyspell
  :bind (:map flyspell-mode-map
              ("C-;" . flyspell-popup-correct)))

;; languagetool

;; [[https://www.languagetool.org/][LanguageTool]] is an Open Source proof­reading program for English, French,
;; German, Polish, and more than 20 other languages.

(use-package langtool
  :bind (("C-x 4 w" . langtool-check)                   ; check buffer and show warnings
         ("C-x 4 W" . langtool-check-done)              ; finish checking and remove markers
         ("C-x 4 l" . langtool-switch-default-language) ; swicth languages
         ("C-x 4 n" . langtool-goto-next-error)         ; go to the next error
         ("C-x 4 4" . langtool-show-message-at-point)   ; show the warning at point
         ("C-x 4 c" . langtool-correct-buffer)          ; correct markers
         )
  :config
  (validate-setq langtool-language-tool-jar "/usr/share/languagetool/languagetool-commandline.jar"
                 langtool-java-bin "/usr/bin/java"
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
                 'langtool-autoshow-detail-popup)
  )

;; writegood

;; [[https://github.com/bnbeckwith/writegood-mode][Writegood]] is a minor mode to aid in finding common writing problems. It
;; highlights text based on a set of weasel-words, passive-voice and duplicate
;; words. [[http://matt.might.net/articles/shell-scripts-for-passive-voice-weasel-words-duplicates/][Matt Might’s weaselwords scripts]] inspired this mode.

(use-package writegood-mode
  :disabled t
  :config
  (progn
    (add-hook 'org-mode-hook      'writegood-mode)
    (add-hook 'text-mode-hook     'writegood-mode)
    (add-hook 'markdown-mode-hook 'writegood-mode)))

;; indentation

;; - [[https://github.com/Malabarba/aggressive-indent-mode][agressive-indent-mode]] keeps the code always indented.
;; - [[https://github.com/DarthFennec/highlight-indent-guides][highligh-indent-guides]] minor mode to highlight indentation. I prefer it over
;;   [[https://github.com/zk-phi/indent-guide][indent-guide]] (too slow when I tested it)

(use-package aggressive-indent
  :demand t
  :config
  (global-aggressive-indent-mode 1)
  (add-to-list 'aggressive-indent-excluded-modes 'html-mode)
  )

;; Leaving disabled for now. It does
(use-package highlight-indent-guides
  :disabled t
  :config
  (add-hook 'prog-mode-hook 'highlight-indent-guides-mode)
  (setq highlight-indent-guides-method 'column)
  ;;(set-face-background 'highlight-indent-guides-odd-face "#3f3f39")
  ;;(set-face-background 'highlight-indent-guides-even-face "#32322d")
  )

;; parenthesis and delimiters

;; - [[https://github.com/Fuco1/smartparens][smartparens]] is a minor mode that deals with parens pairs and tries to be smart
;;   about it. There are good tips [[https://ebzzry.github.io/emacs-pairs.html][here]].

;; - [[https://github.com/Fanael/rainbow-delimiters][rainbow-delimiters]] puts different colours on parenthesis depending on their depth.

(use-package smartparens-config
    :ensure smartparens
    :config
      (show-smartparens-global-mode t))

(add-hook 'prog-mode-hook 'turn-on-smartparens-strict-mode)
(add-hook 'markdown-mode-hook 'turn-on-smartparens-strict-mode)

(use-package rainbow-delimiters
  :demand t
  :config
  (add-hook 'prog-mode-hook #'rainbow-delimiters-mode))

;; ascii-doc

;; [[http://www.methods.co.nz/asciidoc/][AsciiDoc]] is a text document format for writing short documents, articles, books
;; and UNIX man pages. AsciiDoc files can be translated to HTML and DocBook
;; markups.

;; [[https://github.com/sensorflo/adoc-mode][adoc-mode]] is an Emacs major mode for editing AsciiDoc files. It emphasizes on
;; the idea that the document is highlighted so it pretty much looks like the final
;; output. What must be bold is bold, what must be italic is italic etc. Meta
;; characters are naturally still visible, but in a faint way, so they can be
;; easily ignored.

(use-package adoc-mode
  :config
  (autoload 'adoc-mode "adoc-mode" nil t))

;; jinja2

(use-package jinja2-mode
  :mode "\\.j2\\'")

;; json-mode

;; Installs json-mode and make its reformat keybinding match the global default.

(use-package json-mode
  :commands json-mode
  :config
  (bind-keys :map json-mode-map
             ("C-c <tab>" . json-mode-beautify)))

;; markdown

(use-package markdown-mode)

;; slime

(use-package slime
  :config
  (setq inferior-lisp-program "sbcl")
  (load (expand-file-name "~/quicklisp/slime-helper.el")))

(use-package elisp-slime-nav
  :after slime
  :config
  (dolist (hook '(emacs-lisp-mode-hook ielm-mode-hook))
    (add-hook hook 'turn-on-elisp-slime-nav-mode)))

;; yaml

(use-package yaml-mode)

;; eshell

;; [[https://github.com/dakrone/eos/blob/master/eos-shell.org][source]]

;; My frustration with shells makes me enjoy Emacs Shell, but there are some
;; significant differences to address. To this end, I [[http://www.howardism.org/Technical/Emacs/eshell-fun.html][documented most features]].

;; The ~keychain-environmet~ is to be used together with [[http://www.funtoo.org/Keychain][keychain]]. It loads the
;; file "$HOME/.keychain/$HOSTNAME-sh" and parses it for the SSH_AUTH_SOCK and
;; SSH_AUTH_PID variables.

(use-package keychain-environment
  :config
  (keychain-refresh-environment))

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

(add-hook 'eshell-mode-hook (lambda ()
                              (setq-local show-trailing-whitespace nil)
                              (semantic-mode -1)
                              (hl-line-mode -1)
                              (global-hl-line-mode -1)))

;; Define a pretty prompt.
(use-package eshell-git-prompt
  :config
  (eshell-git-prompt-use-theme 'powerline))

;; Like Plan-9 shell
(use-package em-smart
  :ensure nil
  :init
  (add-hook 'eshell-mode-hook 'eshell-smart-initialize)
  :config
  (setq eshell-where-to-jump 'begin
        eshell-review-quick-commands nil
        eshell-smart-space-goes-to-end t))

;; eshell history with counsel

;; Navigate eshell history using counsel. Keybinding to `C-c C-l` - [[http://informatica.boccaperta.com/m-x-emacs-history-di-eshell-con-counsel/][Boccaperta]]

;; FIXME: There's a bug on Eshell that forces the key-binding to be include using
;; the `add-hook` below (not nice!). As soon as the bug is fixed, add the
;; key-binding using `bind` from use-package:

;; :bind (:map eshell-mode-map
;;             ("C-c C-l" . mu-counsel-esh-history))

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

;; tramp

(setq tramp-default-method "ssh")

;; ansible

(use-package ansible
  :init
  (add-hook 'yaml-mode-hook '(lambda () (ansible 1)))
  )

(use-package ansible-doc
  :after ansible
  :init
  (add-hook 'yaml-mode-hook #'ansible-doc-mode)
  )

;; puppet

(use-package puppet-mode)

;; vagrant

(use-package vagrant)

(use-package vagrant-tramp
  :after vagrant
  :config
  (eval-after-load 'tramp '(vagrant-tramp-enable)))

;; Themes

;; Some interesting themes are listed (just change it below to load and check how
;; it looks). At first I was loading all of them with ~use-package~, but they were
;; interfering with each other. I think it's safer loading just the chosen one.

;; - [[https://github.com/jordonbiondo/ample-theme][ample-themes]]
;; - [[https://github.com/waymondo/apropospriate-theme][apropospriate-theme]]
;; - [[https://github.com/cpaulik/emacs-material-theme][material-theme]]
;; - [[https://github.com/oneKelvinSmith/monokai-emacs][monokai-theme]]
;; - [[https://github.com/purcell/color-theme-sanityinc-tomorrow][sanityinc-tomorrow]]
;; - [[https://github.com/bbatsov/solarized-emacs][solarized-theme]]
;; - [[https://github.com/fniessen/emacs-leuven-theme][leuven-theme]]
;; - [[https://github.com/bbatsov/zenburn-emacs][zenburn-theme]]
;; - [[https://github.com/nashamri/spacemacs-theme][spacemacs-theme]]

(use-package leuven-theme
  :disabled t
  :config
  (load-theme 'leuven t)
  (setq leuven-scale-outline-headlines nil))

(use-package doom-themes
  :disabled t
  :config
  (load-theme 'doom-one t)
  (setq doom-enable-brighter-comments t)      ; comments are easy to see
  (add-hook 'find-file-hook
            'doom-buffer-mode)                ; brighter source buffers
  (add-hook 'minibuffer-setup-hook
            'doom-brighten-minibuffer)        ; brighter minibuffer when active
  (require 'doom-neotree))                    ; custom neotree theme

(use-package dracula-theme
  :disabled t
  :config
  (load-theme 'dracula t))

(use-package monokai-theme
  :disabled t
  :config
  (load-theme 'monokai t))

(use-package zerodark-theme
  :disabled t
  :config
  (load-theme 'zerodark t)
  (zerodark-setup-modeline-format-alt))

(use-package material-theme
  :demand t
  :config
  (load-theme 'material t))

;; Fine tuning of the theme

;; Some themes, as solarized and material, change the pitch size of org-headers. I
;; think it's a little to big, so I adjust them here. In this case I'm adjusting
;; the ~material-theme~, which is the one I'm using. If you want to change the
;; ~solarized-theme~ instead, check [[https://github.com/bbatsov/solarized-emacs#theme-specific-settings][here]].

(custom-theme-set-faces
 'material
 `(org-level-1 ((t (:inherit outline-1
                             :background ,"#455A64"
                             :weight bold
                             :box (:style released-button)
                             :height 1.1))))
 `(org-level-2 ((t (:inherit outline-2
                             :background ,"#35575b"
                             :box (:style released-button)
                             :height 1.0))))
 `(org-level-3 ((t (:inherit outline-3 :height 1.0))))
 `(org-level-4 ((t (:inherit outline-4 :height 1.0))))
 `(org-level-5 ((t (:inherit outline-5 ))))
 `(org-level-6 ((t (:inherit outline-6 ))))
 `(org-level-7 ((t (:inherit outline-7 ))))
 `(org-level-8 ((t (:inherit outline-8 ))))
 `(org-level-9 ((t (:inherit outline-9 ))))
 )

;; Fonts

;; (cond ((eq system-type 'gnu/linux)
;;        (set-face-attribute 'default nil
;;                            :family "Source Code Pro"
;;                            :height 90))

;;       ((eq system-type 'darwin)
;;        (set-face-attribute 'default nil
;;                            :family "Source Code Pro"
;;                            :height 100)))

;; Set a smaller font for the mode line
;; (set-face-attribute 'mode-line nil
;;                     :family "Source Code Pro"
;;                     :height 90)

;; Set a font with great support for Unicode Symbols to fallback in
;; those case where certain Unicode glyphs are missing in the
;; current font. Test range: 🐷 ❤ ⊄ ∫ 𝛼 α 🜚 Ⓚ
;; (set-fontset-font "fontset-default" nil
;;                   (font-spec :size 20 :name "Symbola"))

;;  (set-fontset-font "fontset-default" 'unicode "Dejavu Sans Mono")


(set-face-attribute 'default nil
                    :family "Source Code Pro" :height 90)
(set-face-attribute 'variable-pitch nil
                    :family "Fira Sans" :height 100 :weight 'regular)

;; Font setup
(defun my-configure-fonts (frame)
  "Set up fonts for FRAME.
Set the default font, and configure various overrides for
symbols, emojis, greek letters, as well as fall backs for."
  ;; Additional fonts for special characters and fallbacks
  ;; Test range: 🐷 ❤ ⊄ ∫ 𝛼 α 🜚 Ⓚ
  (dolist (script '(symbol mathematical))
    (set-fontset-font t script (font-spec :family "XITS Math")
                      frame 'prepend))

  ;; Define a font set stack for symbols, greek and math characters
  (dolist (script '(symbol greek mathematical))
    (set-fontset-font t script (font-spec :family "Arial Unicode MS")
                      frame 'prepend)
    (set-fontset-font t script (font-spec :family "Menlo")
                      frame 'prepend)
    (set-fontset-font t script (font-spec :family "DejaVu Sans Mono")
                      frame 'prepend))

  (when (eq system-type 'darwin)
    ;; Colored Emoji on OS X, prefer over everything else!
    (set-fontset-font t nil (font-spec :family "Apple Color Emoji")
                      frame 'prepend))

  ;; Fallbacks for math and generic symbols
  (set-fontset-font t nil (font-spec :family "Apple Symbols")
                    frame 'append))

(when-let (frame (selected-frame))
  (my-configure-fonts frame))
(add-hook 'after-make-frame-functions #'my-configure-fonts)


  (use-package unicode-fonts
    :demand t
    :config
    (unicode-fonts-setup))

;; all-the-icons

;; [[https://github.com/domtronn/all-the-icons.el][all-the-icons]] is a utility package to collect various Icon Fonts and propertize
;; them within Emacs.

(use-package all-the-icons
  :demand t)

;; delight

;; Delight enables you to easily customise how major and minor modes appear in the
;; ModeLine.

;; It is similar in purpose to DiminishedModes but it accounts for major modes as
;; well as minor modes, and also incorporates the necessary ‘eval-after-load’ call
;; for minor modes, which makes the configuration simpler.

(use-package delight
  :demand t
  :config
  (delight '((company-mode " Ⓐ" company)
             (hs-minor-mode " ⓗ" hideshow)
             (outline-minor-mode " Ⓞ" outline)
             (outline-mode " Ⓞ" :major)
             (git-gutter-mode " Ⓖ" git-gutter)
             (flyspell-mode " Ⓕ" flyspell)
             (smartparens-mode " Ⓢ" smartparens)
             (elisp-slime-nav-mode nil elisp-slime-nav)
             (emacs-lisp-mode "Elisp" :major)
             (lisp-interaction-mode "LispI" :major)
             (ess-noweb-font-lock-mode nil ess)
             (reftex-mode " Ⓡ" reftex)
             (visual-line-mode " Ⓦ" simple)
             (ess-noweb-mode " Ⓝ" ess)
             (anzu-mode " Ⓩ" anzu)
             (abbrev-mode " ⓐ" abbrev)
             (helm-mode " Ⓗ" helm)
             (rainbow-mode)
             (org-indent-mode nil org-indent)
             (which-key-mode nil which-key)
             (counsel-mode nil counsel)
             (ivy-mode nil ivy)
             (fixmee-mode nil fixmee)
             (button-lock-mode nil button-lock)
             (beacon-mode nil beacon)
             (page-break-lines-mode nil page-break-lines)
             (auto-revert-mode nil autorevert)
             (undo-tree-mode nil undo-tree)
             ;;(server-buffer-clients . " ⓒ")
             )))

;; spaceline

(use-package spaceline
  :config
  (setq powerline-default-separator 'wave
        spaceline-window-numbers-unicode t
        spaceline-workspace-numbers-unicode t))

(use-package spaceline-config
  :demand t
  :ensure nil
  :config
  (spaceline-spacemacs-theme))

;; Other details

;; Better looking break lines.

(use-package page-break-lines
  :init (global-page-break-lines-mode))
