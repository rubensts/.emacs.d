;;; -*- lexical-binding: t -*-

(use-package org
  :straight org-plus-contrib
  :config
  (setq
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
    ("✘ CANCEL" . (:foreground "#00bfff" :weight bold))))

;; org-babel
(use-package ob
  :straight nil
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

;; org-clock
(use-package org-clock
  :straight nil
  :config
  (org-clock-persistence-insinuate)           ; resume clocking task when emacs is restarted
  (setq
   org-clock-persist t                        ; save all clock history when exiting Emacs, load it on startup
   org-clock-persist-query-resume nil         ; do not prompt to resume an active clock
   org-clock-history-length 10                ; show lot of clocking history from where choose items
   org-clock-in-resume t                      ; resume clocking task on clock-in if the clock is open
   org-clock-clocked-in-display nil           ; don't show current task in the modeline
   org-clock-into-drawer "CLOCKING"           ; clocking goes into specfic drawer
   org-clock-report-include-clocking-task t)) ; include current clocking task in clock reports

;; counsel-org-clock
(use-package counsel-org-clock
  :straight (counsel-org-clock :type git
                               :host github
                               :repo "akirak/counsel-org-clock"))

(provide 'ext-org)
;;; ext-org.el ends here
