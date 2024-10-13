;; * emacs essentials

(use-package emacs
  :ensure nil
  
  :custom
  (auto-save-default nil) ;; Disable automatic saving of buffers.
  (use-short-answers t) ;; Use short answers in prompts
  (use-dialog-box nil) ;; Disable dialog boxes in favor of minibuffer prompts.
  (delete-by-moving-to-trash t) ;; Move deleted files to the trash instead of permanently deleting them.
  (delete-selection-mode 1) ;; Enable replacing selected text with typed text.
  (display-line-numbers-type 'relative) ;; Use relative line numbering in programming modes.
  (global-auto-revert-non-file-buffers t) ;; Automatically refresh non-file buffers.
  (split-width-threshold 300) ;; Prevent window splitting if the window width exceeds 300 pixels.
  (ispell-dictionary "en_GB") ;; Set the default dictionary for spell checking.
  (column-number-mode t) ;; Display the column number in the mode line.
  (tab-always-indent 'complete) ;; Make the TAB key complete text instead of just indenting.
  (tab-width 4) ;; Set the tab width to 4 spaces.
  
  ;:hook
  ;(prog-mode . display-line-numbers-mode) ;; Enable line numbers in programming modes.
  
  :config
  ;; Make sure ] b and [ b will always load a file buffer.
  (defun skip-these-buffers (_window buffer _bury-or-kill)
  "Function for `switch-to-prev-buffer-skip'."
  (string-match "\\*[^*]+\\*" (buffer-name buffer)))
  (setq switch-to-prev-buffer-skip 'skip-these-buffers)
  
  ;; Custom file
  (setq custom-file (locate-user-emacs-file "custom-vars.el")) ;; Specify the custom file path.
  (load custom-file 'noerror 'nomessage) ;; Load the custom file quietly, ignoring errors.
  
  :init
  (setq enable-recursive-minibuffers t)
  (setq backup-by-copying t)
  (setq sentence-end-double-space nil)
  (setq frame-inhibit-implied-resize t) ;; useless for a tiling window manager
  (setq show-trailing-whitespace t) ;; self-explanatory
  
  (setq indent-tabs-mode nil) ;; no tabs
  
  ;; keep backup and save files in a dedicated directory
  (setq backup-directory-alist
    `((".*" . ,(concat user-emacs-directory "backups")))
    auto-save-file-name-transforms
    `((".*" ,(concat user-emacs-directory "backups") t)))
  
  (setq create-lockfiles nil) ;; no need to create lockfiles
  
  (set-charset-priority 'unicode) ;; utf8 everywhere
  (setq locale-coding-system 'utf-8
        coding-system-for-read 'utf-8
        coding-system-for-write 'utf-8)
  (set-terminal-coding-system 'utf-8)
  (set-keyboard-coding-system 'utf-8)
  (set-selection-coding-system 'utf-8)
  (prefer-coding-system 'utf-8)
  (setq default-process-coding-system '(utf-8-unix . utf-8-unix))
  
  (global-set-key (kbd "<escape>") 'keyboard-escape-quit) ;; escape quits everything
  
  ;; less noise when compiling elisp
  (setq byte-compile-warnings '(not free-vars unresolved noruntime lexical make-local))
  (setq native-comp-async-report-warnings-errors nil)
  (setq load-prefer-newer t)
  
  (show-paren-mode t)
  
  ;; Hide commands in M-x which don't work in the current mode
  (setq read-extended-command-predicate #'command-completion-default-include-p))
  
  (global-hl-line-mode 0)              ;; Enable highlight of the current line
  (global-display-line-numbers-mode 1) ;; Display line-numbers mode globally.
  (global-visual-line-mode t)          ;; Visual-Line mode in all buffers.
  (global-auto-revert-mode 1)          ;; Enable global auto-revert mode.
  (recentf-mode 1)                     ;; Enable tracking of recently opened files.
  (savehist-mode 1)                    ;; Enable saving of command history.
  (save-place-mode 1)                  ;; Enable saving the place in files for easier return.
  (winner-mode)                        ;; Enable winner mode to easily undo window config changes.
  (xterm-mouse-mode 1)                 ;; Enable mouse support in terminal mode.
  (file-name-shadow-mode 1)            ;; Enable shadowing of filenames for clarity.

  (modify-coding-system-alist 'file "" 'utf-8)

  ;; Add a hook to run code after Emacs has fully initialized.
  (add-hook 'after-init-hook
    (lambda ()
      (message "Emacs has fully loaded. This code runs after startup.")

      ;; Insert a welcome message in the *scratch* buffer displaying loading time and activated packages.
      (with-current-buffer (get-buffer-create "*scratch*")
        (insert (format
                 ";;    Welcome to Emacs!
;;
;;    Loading time : %s
;;    Packages     : %s
"
                  (emacs-init-time)
                  (number-to-string (length package-activated-list)))))))

;; ** disable line numbers in certain contexts

  (dolist (mode 
           '(org-mode-hook
             term-mode-hook
             shell-mode-hook
             eshell-mode-hook
             dashboard-mode-hook
             typst-ts-mode-hook))
    (add-hook mode (lambda () (display-line-numbers-mode 0))))

;; * provide module
(provide 'gato-emacs-essentials)
