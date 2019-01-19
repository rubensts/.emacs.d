;;; base-os.el --- OS customizations -*- lexical-binding: t -*-

;; Check which OS is hosting Emacs to keep compatibility
(defun system-is-mac ()
  (interactive)
  (string-equal system-type "darwin"))

(defun system-is-linux ()
  (interactive)
  (string-equal system-type "gnu/linux"))

(use-package exec-path-from-shell
  :if (and (eq system-type 'darwin) (display-graphic-p))
  :config
  (progn
    (when (string-match-p "/zsh$" (getenv "SHELL"))
      (setq exec-path-from-shell-arguments '("-l")))
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
        (message "Command is now bound to SUPER and Option is bound to META.")))))

(provide 'base-os)
;;; base-os.el ends here
