;;; -*- lexical-binding: t -*-

;;; Magit
(use-package magit
  :hook (after-save . magit-after-save-refresh-status) ; refreshes magit status when file is saved
  :config
  (setq magit-completing-read-function 'ivy-completing-read
        magit-display-buffer-function 'magit-display-buffer-fullframe-status-topleft-v1))

;;; git-gutter
(use-package git-gutter
  :init
  (global-git-gutter-mode +1))

;;; git-timemachine
(use-package git-timemachine
  :commands git-timemachine)

;;; ghub
(use-package ghub)

(provide 'ext-magit)
;;; ext-magit.el ends here
