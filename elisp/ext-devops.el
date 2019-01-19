;;; ext-devops.el --- Packages that help devops -*- lexical-binding: t -*-
;;; Commentary:
;;; Code:

;; tramp
(setq tramp-default-method "ssh")

;; ansible
(use-package ansible
  :init
  (add-hook 'yaml-mode-hook '(lambda () (ansible 1))))

(use-package ansible-doc
  :after ansible
  :init
  (add-hook 'yaml-mode-hook #'ansible-doc-mode))

;; docker
(use-package dockerfile-mode)
(use-package docker-compose-mode)
(use-package docker-tramp)

;; puppet
(use-package puppet-mode)

;; vagrant
(use-package vagrant)

(use-package vagrant-tramp
  :after vagrant
  :config
  (eval-after-load 'tramp '(vagrant-tramp-add-method)))

;;ztree
(use-package ztree)

(provide 'ext-devops)
;;; ext-devops.el ends here
