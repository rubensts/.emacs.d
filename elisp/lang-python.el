;;; lang-python.el --- Python editing -*- lexical-binding: t -*-
;;; Commentary:
;;; Code:

;; (use-package anaconda-mode
;;   :hook ((python-mode . anaconda-mode)
;;          (python-mode . anaconda-eldoc-mode))
;;   :init
;;   (add-to-list 'auto-mode-alist '("\\.py$" . python-mode)))

(use-package lpy
  :straight (:host github :repo "abo-abo/lpy")
  :hook ((python-mode . lpy-mode))
  :init
  (add-to-list 'auto-mode-alist '("\\.py$" . python-mode)))


(provide 'lang-python)
;;; lang-python.el ends here
