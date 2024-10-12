;; * eldoc

(use-package eldoc
  :ensure nil          ;; This is built-in, no need to fetch it.
  :init
  (global-eldoc-mode)) 

;; * provide module
(provide 'gato-emacs-eldoc)
