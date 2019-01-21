;;; -*- lexical-binding: t -*-

(setq visual-line-fringe-indicators '(left-curly-arrow right-curly-arrow))
(fringe-mode 16)

(use-package spacemacs-theme
  :defer t
  :custom
  (spacemacs-theme-comment-bg nil)
  (spacemacs-theme-comment-italic t))
;; If I ever want to use original comment color with italics together,
;; I should uncomment these 2 lines.
;; See https://github.com/nashamri/spacemacs-theme/issues/104
;; (custom-set-variables '(spacemacs-theme-custom-colors
;;                         '((comment-light . "#2aa1ae"))))

(use-package doom-themes
  :defer t
  :init
  (setq doom-themes-enable-bold t     ; bold is universally enable
        doom-themes-enable-italic t)  ; italics is universally enabled
  :config
  (doom-themes-visual-bell-config)  ; flashes mode-line on errors
  (doom-themes-org-config))         ; corrects (and improves) org-mode's native fontification

;; (use-package poet-theme
;;   :defer t
;;   :hook (text-mode . variable-pitch-mode))

(use-package heaven-and-hell
  :init
  (setq heaven-and-hell-theme-type 'dark)
  (setq heaven-and-hell-themes
	'((light . doom-solarized-light)
	  (dark . doom-one)))
  :hook (after-init  . heaven-and-hell-init-hook)
  :bind (("C-c <f6>" . heaven-and-hell-load-default-theme)
	 ("<f6>"     . heaven-and-hell-toggle-theme)))

(use-package all-the-icons
  :config
  ;; all-the-icons doesn't work without font-lock+
  ;; And font-lock+ doesn't have autoloads
  (use-package font-lock+
    :straight (:host github :repo "emacsmirror/font-lock-plus")
    :config (require 'font-lock+)))

(provide 'appearance-theme)
;;; appearance-theme.el ends here
