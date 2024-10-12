;; * projectile

(use-package projectile
  :defer t
  :hook
  (elpaca-after-init . projectile-mode))

;; * provide module
(provide 'gato-emacs-projectile)
