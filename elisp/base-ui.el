;;; -*- lexical-binding: t -*-

(setq visual-line-fringe-indicators '(left-curly-arrow right-curly-arrow))
(fringe-mode 16)

(setq my/font-name "SauceCodePro Nerd Font")
(defcustom my/font-size 12 "My default font size")

;; (set-face-attribute 'fixed-pitch nil :family "SauceCodePro Nerd Font")
;; (set-farce-attribute 'variable-pitch nil :family "Baskerville")

(defun set-frame-font-size (&optional font-size)
  "Change fram font size to FONT-SIZE.
If no FONT-SIZE provided, reset the font size to its default variable."
  (let ((font-size
	     (or font-size
	         (eval (car (get 'my/font-size 'standard-value))))))
    (customize-set-variable 'my/font-size font-size)
    (set-frame-font
     (format "%s %d" my/font-name font-size) nil t)))

(defun increase-frame-font ()
  "Increase frame font by one."
  (interactive)
  (set-frame-font-size (+ my/font-size 1)))

(defun decrease-frame-font ()
  "Decrease frame font by one."
  (interactive)
  (set-frame-font-size (- my/font-size 1)))

(defun reset-frame-font ()
  "Reset frame font to its default value."
  (interactive)
  (set-frame-font-size))

;; Probably good case for a hydra
(global-set-key (kbd "C-x C-0") 'reset-frame-font)
(global-set-key (kbd "C-x C--") 'decrease-frame-font)
(global-set-key (kbd "C-x C-=") 'increase-frame-font)
(global-set-key (kbd "C-x C-+") 'text-scale-adjust)

(add-hook 'after-init-hook 'reset-frame-font)

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

(use-package spaceline)

(use-package spaceline-config
  :straight spaceline
  ;; :init
  ;; (setq powerline-default-separator 'wave)
  :config
  (spaceline-spacemacs-theme)
  (spaceline-toggle-minor-modes-off))

(use-package spaceline-all-the-icons
  ;;:disabled t
  :after spaceline
  :config
  (spaceline-all-the-icons-theme)
  (spaceline-all-the-icons--setup-anzu)
  (spaceline-all-the-icons--setup-git-ahead)
  (spaceline-toggle-projectile-root-off)
  (spaceline-toggle-all-the-icons-projectile-off)
  (spaceline-toggle-all-the-icons-buffer-id-off))

;; (use-package doom-modeline
;;   :defer t
;;   :hook (after-init . doom-modeline-init))

(provide 'base-ui)
;;; base-ui.el ends here
