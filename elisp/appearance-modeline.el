;;; -*- lexical-binding: t -*-

(use-package doom-modeline
  :defer t
  :hook (after-init . doom-modeline-mode)
  :config
  (setq doom-modeline-height 20
        doom-modeline-github t
        doom-modeline-major-mode-color-icon t
        doom-modeline-buffer-file-name-style 'buffer-name))

(provide 'appearance-modeline)
;;; appearance-modeline.el ends here
