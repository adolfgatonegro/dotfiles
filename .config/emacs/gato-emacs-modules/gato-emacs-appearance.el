;; * fonts

;; define default, variable pitch, and fixed pitch fonts
(set-face-attribute 'default nil
  :font "monospace"
  :height 90)
(set-face-attribute 'variable-pitch nil
  :font "sans-serif"
  :height 90
  :weight 'medium)
(set-face-attribute 'fixed-pitch nil
  :font "monospace"
  :height 90)

;; display commented text and keywords in italics
;; requires a font with italics support
(set-face-attribute 'font-lock-comment-face nil
  :slant 'italic)
(set-face-attribute 'font-lock-keyword-face nil
  :slant 'italic)

;; adjust line spacing
(setq-default line-spacing 0.15)

;; * icons

;; ** nerd icons
(use-package nerd-icons
  :defer t)

;; ** nerd-icons-dired
(use-package nerd-icons-dired
  :defer t
  :hook
  (dired-mode . nerd-icons-dired-mode))

;; ** nerd-icons-completion
(use-package nerd-icons-completion
  :after (:all nerd-icons marginalia)
  :config
  (nerd-icons-completion-mode)
  (add-hook 'marginalia-mode-hook #'nerd-icons-completion-marginalia-setup))

;; * theme

;; ** add local themes to path
(add-to-list 'custom-theme-load-path (concat user-emacs-directory "themes"))

;; ** doomemacs/themes
(use-package doom-themes
  :custom
  (doom-themes-enable-bold t)
  (doom-themes-enable-italic t)
  :config
  (load-theme 'doom-gatonegro t)
  (doom-themes-visual-bell-config)
  (doom-themes-org-config))

;; * modeline
(use-package doom-modeline
  :defer t
  :config
  (setq doom-modeline-icon t        ; enable icons
        doom-modeline-bar-width 5   ; set the bar width
        doom-modeline-height 35     ; set modeline height
        doom-modeline-persp-icon t  ; add perspective name to modeline
        doom-modeline-persp-name t) ; add folder icon next to persp name
  :hook
  (elpaca-after-init . doom-modeline-mode))

;; * visual-fill-column
(use-package visual-fill-column
  :defer t
  :custom
  (visual-fill-column-width 80)
  (visual-fill-column-adjust-for-text-scale nil)
  :config
  (setq-default visual-fill-column-center-text t))

;; * provide module
(provide 'gato-emacs-appearance)
