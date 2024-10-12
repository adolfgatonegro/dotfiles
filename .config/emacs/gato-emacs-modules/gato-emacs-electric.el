;; * electric

(use-package electric
  :ensure nil
  :init
  (electric-pair-mode +1) ;; automatically insert closing parens
  (electric-indent-mode -1) ;; weird and inconsistent as hell, go away
  (setq electric-pair-preserve-balance t
        org-edit-src-content-indentation 0))

;; * provide module
(provide 'gato-emacs-electric)
