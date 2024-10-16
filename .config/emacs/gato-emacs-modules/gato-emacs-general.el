;; * general

(use-package general
  :config
  (general-evil-setup) ;; integrate general with evil

  ;; set up 'SPC' as the global leader key
  (general-create-definer gato/leader-keys
    :states '(normal insert visual emacs)
    :keymaps 'override
    :prefix "SPC" ;; set leader
    :global-prefix "M-SPC") ;; access leader in insert mode

  ;; set up ',' as the local leader key
  (general-create-definer gato/local-leader-keys
    :states '(normal insert visual emacs)
    :keymaps 'override
    :prefix "," ;; set local leader
    :global-prefix "M-,") ;; access local leader in insert mode

  ;; Buffers
  (gato/leader-keys
    "b" '(:ignore t :wk "Buffer")
    "b b" '(switch-to-buffer :wk "Switch buffer")
    "b i" '(ibuffer :wk "Ibuffer")
    "b k" '(kill-this-buffer :wk "Kill current buffer")
    "b n" '(next-buffer :wk "Next buffer")
    "b p" '(previous-buffer :wk "Previous buffer")
    "b r" '(revert-buffer :wk "Revert buffer"))

  ;; Extended command
  (gato/leader-keys
    "SPC" '(execute-extended-command :wk "Execute extended command"))

  ;; Diff-hl
  (general-define-key
    :states 'normal
    "] c" '(diff-hl-next-hunk :wk "Next diff hunk")
    "[ c" '(diff-hl-previous-hunk :wk "Previous diff hunk"))

  ;; Dired
  (gato/leader-keys
    "d" '(:ignore t :wk "dired")
    "d d" '(dired :wk "Open dired")
    "d j" '(dired-jump :wk "Dired jump to current")
    "d w" '(:ignore t :wk "Writable dired")
    "d w w" '(wdired-change-to-wdired-mode :wk "Enable writable dired")
    "d w a" '(wdired-abort-changes :wk "Abort writable dired changes")
    "d w f" '(wdired-finish-edit :wk "Finish writable dired edit"))

  (general-define-key
    :states 'normal
    :keymaps 'dired-mode-map
    "M-RET" 'dired-display-file
    "h" 'dired-up-directory
    "l" 'dired-open-file
    "m" 'dired-mark
    "t" 'dired-toggle-marks
    "u" 'dired-unmark
    "p" 'dired-preview-mode
    "v" 'dired-view-file
    "C" 'dired-do-copy
    "D" 'dired-do-delete
    "J" 'dired-goto-file
    "M" 'dired-do-chmod
    "O" 'dired-do-chown
    "P" 'dired-do-print
    "R" 'dired-do-rename
    "T" 'dired-do-touch
    "Z" 'dired-do-compress
    "+" 'dired-create-directory
    "-" 'dired-up-directory
    "% l" 'dired-downcase
    "% m" 'dired-mark-files-regexp
    "% u" 'dired-upcase
    "* %" 'dired-mark-files-regexp
    "* ." 'dired-mark-extension
    "* /" 'dired-mark-directories
  )

  ;; Eshell / Evaluate Elisp
  (gato/leader-keys
    "e" '(:ignore t :wk "Eshell / Evaluate")
    ;; Evaluate
    "e b" '(eval-buffer :wk "Evaluate Elisp in buffer")
    "e d" '(eval-defun :wk "Evaluate defun containing or after point")
    "e e" '(eval-expression :wk "Evaluate Elisp expression")
    "e l" '(eval-last-sexp :wk "Evaluate Elisp expression before point")
    "e r" '(eval-region :wk "Evaluate Elisp in region")
    ;; Eshell
    "e s" '(eshell :wk "Eshell"))

  ;; Find files
  (gato/leader-keys
    "." '(find-file :wk "Find file")
    "f c" '((lambda () (interactive) (find-file "~/.config/emacs/init.el")) :wk "Edit Emacs config")
    "f r" '(recentf :wk "Find recent files")
    "f u" '(sudo-edit-find-file :wk "Sudo find file")
    "f U" '(sudo-edit :wk "Sudo edit this file"))

  ;; Help functions
  (gato/leader-keys
   "h" '(:ignore t :wk "Help")
   "h f" '(describe-function :wk "Describe function")
   "h t" '(load-theme :wk "Load theme")
   "h v" '(describe-variable :wk "Describe variable")
   "h r" '((lambda () (interactive) (load-file user-init-file)) :wk "Reload Emacs config")
  )

  ;; Org mode
  (gato/leader-keys
    "m" '(:ignore t :wk "Org")
    "m a" '(org-agenda :wk "Org agenda")
    "m e" '(org-export-dispatch :wk "Org export dispatch")
    "m i" '(org-toggle-item :wk "Org toggle item")
    "m h" '(consult-org-heading :wk "Jump to Org heading")
    "m t" '(org-todo :wk "Org todo")
    "m B" '(org-babel-tangle :wk "Org babel tangle")
    "m T" '(org-todo-list :wk "Org todo list"))

  (gato/leader-keys
    "m b" '(:ignore t :wk "Tables")
    "m b -" '(org-table-insert-hline :wk "Insert hline in table"))

  (gato/leader-keys
    "m d" '(:ignore t :wk "Date/deadline")
    "m d t" '(org-time-stamp :wk "Org time stamp"))

  ;; Org mode - Navigation
  (general-define-key
    :states 'normal
    :keymaps 'org-mode-map
    "g h" 'org-previous-visible-heading
    "g j" 'org-forward-heading-same-level
    "g k" 'org-backward-heading-same-level
    "g l" 'org-next-visible-heading
    "M-l" 'org-demote-subtree
    "M-h" 'org-promote-subtree
  )

  ;; Toggle
  (gato/leader-keys
    "t" '(:ignore t :wk "Toggle")
    "t c" '(visual-line-fill-column-mode :wk "Toggle fill column")
    "t h" '(hl-line-mode :wk "Toggle line highlight")
    "t l" '(display-line-numbers-mode :wk "Toggle line numbers")
    "t t" '(visual-line-mode :wk "Toggle truncated lines")
    "t v" '(vterm-toggle :wk "Toggle vterm"))

  ;; Toggle comments in normal and visual mode.
  (general-define-key
    :states '(normal)
    "g c c" '((lambda () (interactive)
        (comment-or-uncomment-region
          (line-beginning-position) (line-end-position)))
          :wk "Toggle comment"))

  (general-define-key
    :states '(visual)
    "g c" '(comment-or-uncomment-region :wk "Toggle comment"))

  ;; Projectile
  (gato/leader-keys
    "j" '(projectile-command-map :wk "Projectile"))

  ;; Typst
  (gato/leader-keys
    :keymaps 'typst-ts-mode-map
    "p" '(:ignore t :wk "Typst")
    "p c" '(typst-ts-compile :wk "Compile")
    "p w" '(typst-ts-watch-mode :wk "Watch")
    "p o" '(typst-ts-mode-preview :wk "Open compiled document")
    "p p" '(typst-ts-compile-and-preview :wk "Compile and preview"))

  ;; undo-tree
  (gato/leader-keys
    :states '(normal)
    "u" '(undo-tree-visualize :wk "Undo-tree visualise"))

  ;; Windows
  (gato/leader-keys
    "w" '(:ignore t :wk "Windows")
    ;; Window splits
    "w c" '(evil-window-delete :wk "Close window")
    "w n" '(evil-window-new :wk "New window")
    "w s" '(evil-window-split :wk "Horizontal split")
    "w v" '(evil-window-vsplit :wk "Vertical split")
    ;; Window motions
    "w h" '(evil-window-left :wk "Window left")
    "w j" '(evil-window-down :wk "Window down")
    "w k" '(evil-window-up :wk "Window up")
    "w l" '(evil-window-right :wk "Window right")
    "w w" '(evil-window-next :wk "Goto next window")
    ;; Move windows
    "w H" '(buf-move-left :wk "Buffer move left")
    "w J" '(buf-move-down :wk "Buffer move down")
    "w K" '(buf-move-up :wk "Buffer move up")
    "w L" '(buf-move-right :wk "Buffer move right"))

  ;; Zoom in/out
  (general-define-key
    "C-M-=" '(text-scale-increase :wk "Increase text scale")
    "C-M--" '(text-scale-decrease :wk "Decrease text scale"))
;; end General keybindings
)

(global-set-key [escape] 'keyboard-escape-quit)

;; * provide module
(provide 'gato-emacs-general)
