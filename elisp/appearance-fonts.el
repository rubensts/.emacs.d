;;; -*- lexical-binding: t -*-

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

(provide 'appearance-fonts)
;;; appearance-fonts.el ends here
