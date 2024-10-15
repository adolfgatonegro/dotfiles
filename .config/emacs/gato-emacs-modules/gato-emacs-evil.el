;; * evil

(use-package evil
  :defer t
  :config
  (evil-set-undo-system 'undo-tree)   ;; Uses the undo-tree package as the default undo system
  (setq evil-leader/in-all-states t)  ;; Make the leader key available in all states.
  (setq evil-want-fine-undo t)        ;; Evil uses finer grain undoing steps

  :init
  (setq evil-want-integration t
    evil-want-keybinding nil
    evil-vsplit-window-right t
    evil-split-window-below t
    evil-respect-visual-line-mode t
    evil-want-Y-yank-to-eol t)

  ;; Unbind SPC and TAB
  (with-eval-after-load 'evil-maps
    (define-key evil-motion-state-map (kbd "SPC") nil)
    (define-key evil-motion-state-map (kbd "TAB") nil))

  :hook
  (elpaca-after-init . evil-mode))

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
