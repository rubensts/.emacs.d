;;; lang-python.el --- Python editing -*- lexical-binding: t -*-
;;; Commentary:
;;; Code:

;; (use-package anaconda-mode
;;   :hook ((python-mode . anaconda-mode)
;;          (python-mode . anaconda-eldoc-mode))
;;   :init
;;   (add-to-list 'auto-mode-alist '("\\.py$" . python-mode)))

;; Python Mode
;; Install:
;;   pip install pyflakes
;;   pip install autopep8
(use-package python
  :straight nil
  :defines gud-pdb-command-name pdb-path
  :config
  ;; Disable readline based native completion
  (setq python-shell-completion-native-enable nil)

  (add-hook 'inferior-python-mode-hook
            (lambda ()
              ;; (bind-key "C-c C-z" #'kill-buffer-and-window inferior-python-mode-map)
              (process-query-on-exit-flag (get-process "Python")))))

(use-package lpy
  :straight (:host github :repo "abo-abo/lpy")
  :hook ((python-mode . lpy-mode))
  :init
  (add-to-list 'auto-mode-alist '("\\.py$" . python-mode)))

;; Format using YAPF
;; Install: pip install yapf
(use-package yapfify
  :hook (python-mode . yapf-mode))

(provide 'lang-python)
;;; lang-python.el ends here
