;; * evil

(use-package evil
  :defer t

  :hook
  (elpaca-after-init . evil-mode)

  :init
  (setq evil-want-integration t
    evil-want-keybinding nil
    evil-vsplit-window-right t
    evil-split-window-below t
    evil-respect-visual-line-mode t))

;; ** evil collection

(use-package evil-collection
  :defer t
  
  :custom
  (evil-collection-want-find-usages-bindings t)

  :init
  (setq evil-collection-mode-list nil)
  (add-to-list 'evil-collection-mode-list 'dashboard)
  (add-to-list 'evil-collection-mode-list 'dired)
  (add-to-list 'evil-collection-mode-list 'ibuffer)
  (add-to-list 'evil-collection-mode-list 'magit)
  (add-to-list 'evil-collection-mode-list '(pdf pdf-view))

  :hook
  (evil-mode . evil-collection-init))

;; * provide module
(provide 'gato-emacs-evil)
