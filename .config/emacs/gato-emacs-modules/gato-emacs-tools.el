;; * tools

;; ** diminish
(use-package diminish)

;; ** rainbow delimiters
(use-package rainbow-delimiters
  :defer t
  :ensure t
  :hook
  (prog-mode . rainbow-delimiters-mode))

;; ** rainbow mode
(use-package rainbow-mode
  :defer t
  :diminish
  :hook org-mode prog-mode)

;; ** sudo-edit
(use-package sudo-edit
  :defer t)

;; ** undo-tree
(use-package undo-tree
  :defer t
  :ensure t
  :hook
  (elpaca-after-init . global-undo-tree-mode)
  :init
  (setq undo-tree-visualizer-timestamps t
        undo-tree-visualizer-diff t
        ;; Increase undo limits to avoid losing history due to Emacs' garbage collection.
        ;; These values can be adjusted based on your needs.
        ;; 10X bump of the undo limits to avoid issues with premature
        ;; Emacs GC which truncates the undo history very aggressively.
        undo-limit 800000                     ;; Limit for undo entries.
        undo-strong-limit 12000000            ;; Strong limit for undo entries.
        undo-outer-limit 120000000)           ;; Outer limit for undo entries.
  :config
  ;; Set the directory where `undo-tree' will save its history files.
  ;; This keeps undo history across sessions, stored in a cache directory.
  (setq undo-tree-history-directory-alist '(("." . "~/.config/emacs/.cache/undo"))))

;; * provide module
(provide 'gato-emacs-tools)
