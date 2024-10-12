;; * tools

;; ** diminish
(use-package diminish)

;; ** rainbow mode
(use-package rainbow-mode
  :defer t
  :diminish
  :hook org-mode prog-mode)

;; ** sudo-edit
(use-package sudo-edit
  :defer t
  :config
    (gato/leader-keys
      "fu" '(sudo-edit-find-file :wk "Sudo find file")
      "fU" '(sudo-edit :wk "Sudo edit this file")))

;; * provide module
(provide 'gato-emacs-tools)
