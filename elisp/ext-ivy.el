;;; -*- lexical-binding: t -*-

;;----------------------------------------------------------------------------
;; ivy
;;----------------------------------------------------------------------------
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
  ;;:chords (("bb" . ivy-switch-buffer))
  :config
  (ivy-mode 1)
  (setq ivy-use-virtual-buffers t         ; list `recentf' and bookmarks as well
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
       "Not completing files currently"))))

;; Hydra bindings for ivy buffer
(use-package ivy-hydra
  :after ivy)

(use-package all-the-icons-ivy
  :after all-the-icons
  :config
  (all-the-icons-ivy-setup))

;; smex order selections accordingly to the most used ones
(use-package smex)

;;----------------------------------------------------------------------------
;; swiper
;;----------------------------------------------------------------------------
(use-package swiper
  :after ivy
  :bind (("C-s" . swiper))
  :config
  ;; always recenter when leaving swiper
  (setq swiper-action-recenter t))

;;----------------------------------------------------------------------------
;; counsel
;;----------------------------------------------------------------------------
(use-package counsel
  :after swiper
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
  (setq  counsel-mode-override-describe-bindings t
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


(provide 'ext-ivy)
;;; ext-ivy.el ends here
