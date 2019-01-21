;;; -*- lexical-binding: t -*-

;;; Base configuration
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
   org-src-tab-acts-natively        t ; TAB acts natively as it was in the language major mode
   org-src-preserve-indentation     t ; preserve indentation when exporting blocks
   org-src-fontify-natively         t ; highlights code-blocks natively
   org-src-window-setup 'current-window ; open code-blocks in the current window
   org-confirm-babel-evaluate     nil ; don't ask for confirmation when compiling code-blocks

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

;;; org-babel
(use-package ob
  :straight nil
  :config
  (org-babel-do-load-languages
   (quote org-babel-load-languages)
   (quote ((calc       . t)
           (clojure    . t)
           (ditaa      . t)
           (dot        . t)
           (emacs-lisp . t)
           (gnuplot    . t)
           (latex      . t)
           (ledger     . t)
           (octave     . t)
           (org        . t)
           (makefile   . t)
           (plantuml   . t)
           (python     . t)
           (R          . t)
           (ruby       . t)
           (shell      . t)
           (sqlite     . t)
           (sql        . t)
           ))))

;;; org-clock
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

;;; org-capture-templates
;; The Protocol Quote (p) and Protocol Link (L) templates work through the use of the org
;; protocol handler found here, which is a Chrome/Firefox addon. Specifically, the
;; Protocol Quote is activated when a text is selected in the site, capturing the text
;; selected (quote). The Protocol Link works when no text is selected, capturing only the
;; site address. Here is a list of the special %-escapes or placeholders used on org
;; templates.
(use-package org-capture
  :straight nil
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

;;; org-bullets
(use-package org-bullets
  :hook (org-mode . (lambda () (org-bullets-mode t)))
  :config
  ;; option 1: '("☯" "☰" "☱" "☲" "☳" "☴" "☵" "☶" "☷")
  ;; option 2: '("♣" "♥" "♠" "♦" "♧" "♡" "♤" "♢")
  ;; option 3: '("☯" "☉" "∞" "◉" "⊚" "☀" "☾" "☥")
  (setq org-bullets-bullet-list '("◉" "☉" "⊚" "○" "∞")))

;;; ox.el
(use-package ox
  :straight nil
  :config
  (setq org-export-with-smart-quotes t
        org-export-allow-bind-keywords t
        org-latex-listings 'minted
        org-latex-packages-alist '(("" "color" t)
                                   ("" "minted" t)
                                   ("" "parskip" t)
                                   ("" "tikz" t)))

  (when (system-is-mac)
    (setq org-ditaa-jar-path "/usr/local/bin/ditaa"))

  (setq org-latex-pdf-process
        '("pdflatex -shell-escape -interaction nonstopmode -output-directory %o %f"
          "pdflatex -shell-escape -interaction nonstopmode -output-directory %o %f"
          "pdflatex -shell-escape -interaction nonstopmode -output-directory %o %f")))

;;; ox-pandoc
;; I’m using ox-pandoc to export org files to all formats Pandoc works with. It only
;; exports org files, in opposite of pandoc-mode, which exports from any source format.
;; The problem is that ox-pandoc needs considerably less configuration and as I usually
;; write everything in org-mode, no need to worry. https://github.com/kawabata/ox-pandoc
;; http://www.rousette.org.uk/blog/archives/org-mode-and-pandoc/ Keeping a lab book with
;; org-mode http://informatica.boccaperta.com/m-x-emacs-ox-pandoc/

(use-package ox-pandoc
  ;;  :after org-plus-contrib
  :config
  (setq org-pandoc-options '((standalone . t)
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

;;; org-brain
(use-package org-brain
  :config
  (push '("b" "Brain" plain (function org-brain-goto-end)
          "* %i%?" :empty-lines 1)
        org-capture-templates)
  (setq org-brain-visualize-default-choices 'all)
  (setq org-brain-title-max-length 12))


(provide 'ext-org)
;;; ext-org.el ends here
