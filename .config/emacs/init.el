;; * init.el
;; gatoneg.ro

;; see also M-x emacs-init-time
(defconst emacs-start-time (current-time))

;; make the modules available here, so we can easily call them further down
(mapc
 (lambda (string)
   (add-to-list 'load-path (locate-user-emacs-file string)))
 '("gato-emacs-modules"))

(require 'gato-emacs-elpaca)
(require 'gato-emacs-essentials)
(require 'gato-emacs-window)

(require 'gato-emacs-dired)
(require 'gato-emacs-electric)
(require 'gato-emacs-isearch)
(require 'gato-emacs-eldoc)

(require 'gato-emacs-buffer-move)
(require 'gato-emacs-projectile)
(require 'gato-emacs-tools)

(require 'gato-emacs-evil)
(require 'gato-emacs-general)
(require 'gato-emacs-which-key)

(require 'gato-emacs-appearance)
(require 'gato-emacs-dashboard)

(require 'gato-emacs-completion)
(require 'gato-emacs-lsp)
(require 'gato-emacs-typst)

(require 'gato-emacs-org-mode)
(require 'gato-emacs-shell)
(require 'gato-emacs-version-control)

(provide 'init)
