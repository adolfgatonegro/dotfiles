;;; early-init.el --- Early Init -*- no-byte-compile: t; lexical-binding: t; -*-

;; Author: Adolf Gatonegro
;; URL: https://github.com/adolfgatonegro

;;; Commentary:
;; Early initialisation file for my GNU Emacs configuration.

;;; Code:

;; Disable `package.el' here, required to use Elpaca later
(setq package-enable-at-startup nil)

;;; Garbage collection
;; Garbage collection significantly affects startup times. This setting delays
;; garbage collection during startup but will be reset later.
(defvar gato-gc-cons-threshold (* 16 1024 1024)
  "The value of `gc-cons-threshold' after Emacs startup.")

(setq gc-cons-threshold most-positive-fixnum)

(add-hook 'elpaca-after-init-hook
          (lambda ()
            (setq gc-cons-threshold gato-gc-cons-threshold)))

(setq byte-compile-warnings '(not obsolete))
(setq native-comp-async-report-warnings-errors 'silent)
(setq read-process-output-max (* 1024 1024 4))
(setq warning-suppress-log-types '((comp) (bytecomp)))

(setopt initial-major-mode 'fundamental-mode)  ; default mode for the *scratch* buffer

;;; Minimal UI
(menu-bar-mode -1)
(tool-bar-mode -1)
(tooltip-mode -1)
(scroll-bar-mode -1)
(pixel-scroll-precision-mode 1) ;; Enable smooth scrolling

(setq inhibit-startup-echo-area-message (user-login-name)) ;; Silence startup message
(advice-add #'display-startup-echo-area-message :override #'ignore)

(setq inhibit-splash-screen t
      use-file-dialog nil
      use-dialog-box nil
      tab-bar-new-button-show nil
      tab-bar-close-button-show nil
      tab-line-close-button-show nil)

;; Default frame configuration
(setq frame-resize-pixelwise t)
(setq default-frame-alist '((fullscreen . maximized)
                            (vertical-scroll-bars . nil)
                            (horizontal-scroll-bars . nil)
                            (background-color . "#1a1a26")
                            (ns-appearance . dark)
                            (ns-transparent-titlebar . t)))

(provide 'early-init)

;;; early-init.el ends here

;;; init.el --- Emacs Init -*- no-byte-compile: t; lexical-binding: t; -*-

;; Author: Adolf Gatonegro
;; URL: https://github.com/adolfgatonegro

;;; Commentary:
;; The main configuration file for Emacs.

;;; Code:

;;; Package management
;; Clone, build, and initialise Elpaca.
(defvar elpaca-installer-version 0.7)
(defvar elpaca-directory (expand-file-name "elpaca/" user-emacs-directory))
(defvar elpaca-builds-directory (expand-file-name "builds/" elpaca-directory))
(defvar elpaca-repos-directory (expand-file-name "repos/" elpaca-directory))
(defvar elpaca-order '(elpaca :repo "https://github.com/progfolio/elpaca.git"
                              :ref nil :depth 1
                              :files (:defaults "elpaca-test.el" (:exclude "extensions"))
                              :build (:not elpaca--activate-package)))
(let* ((repo  (expand-file-name "elpaca/" elpaca-repos-directory))
       (build (expand-file-name "elpaca/" elpaca-builds-directory))
       (order (cdr elpaca-order))
       (default-directory repo))
  (add-to-list 'load-path (if (file-exists-p build) build repo))
  (unless (file-exists-p repo)
    (make-directory repo t)
    (when (< emacs-major-version 28) (require 'subr-x))
    (condition-case-unless-debug err
        (if-let ((buffer (pop-to-buffer-same-window "*elpaca-bootstrap*"))
                 ((zerop (apply #'call-process `("git" nil ,buffer t "clone"
                                                 ,@(when-let ((depth (plist-get order :depth)))
                                                     (list (format "--depth=%d" depth) "--no-single-branch"))
                                                 ,(plist-get order :repo) ,repo))))
                 ((zerop (call-process "git" nil buffer t "checkout"
                                       (or (plist-get order :ref) "--"))))
                 (emacs (concat invocation-directory invocation-name))
                 ((zerop (call-process emacs nil buffer nil "-Q" "-L" "." "--batch"
                                       "--eval" "(byte-recompile-directory \".\" 0 'force)")))
                 ((require 'elpaca))
                 ((elpaca-generate-autoloads "elpaca" repo)))
            (progn (message "%s" (buffer-string)) (kill-buffer buffer))
          (error "%s" (with-current-buffer buffer (buffer-string))))
      ((error) (warn "%s" err) (delete-directory repo 'recursive))))
  (unless (require 'elpaca-autoloads nil t)
    (require 'elpaca)
    (elpaca-generate-autoloads "elpaca" repo)
    (load "./elpaca-autoloads")))
(add-hook 'after-init-hook #'elpaca-process-queues)
(elpaca `(,@elpaca-order))

;; Enable `use-package` support for convenience.
(elpaca elpaca-use-package
  (elpaca-use-package-mode))
;; Always ensure the packages we want are installed.
(setq use-package-always-ensure t)

;;; Configuring Emacs itself
(use-package emacs
  :ensure nil ;; Important for built-in packages.

  :init
  ;; Keep backup and save files in a dedicated directory.
  (setq backup-directory-alist
    `((".*" . ,(concat user-emacs-directory "backups")))
    auto-save-file-name-transforms
    `((".*" ,(concat user-emacs-directory "backups") t)))

  (setq create-lockfiles nil) ;; No need to create lockfiles.

  (set-charset-priority 'unicode) ;; UTF-8 everywhere.
  (setq locale-coding-system 'utf-8
        coding-system-for-read 'utf-8
        coding-system-for-write 'utf-8)
  (set-terminal-coding-system 'utf-8)
  (set-keyboard-coding-system 'utf-8)
  (set-selection-coding-system 'utf-8)
  (prefer-coding-system 'utf-8)
  (setq default-process-coding-system '(utf-8-unix . utf-8-unix))

  :custom
  ;; Better defaults.
  (auto-save-default nil)                 ;; Don't autosave buffers.
  (backup-by-copying t)                   ;; Use copying to create backups.
  (column-number-mode t)                  ;; Display the column number in the mode line.
  (delete-by-moving-to-trash t)           ;; Move deleted files to the trash instead of permanently deleting them.
  (delete-selection-mode 1)               ;; Enable replacing selected text with typed text.
  (frame-inhibit-implied-resize t)        ;; Useless for a tiling window manager.
  (global-auto-revert-mode nil)           ;; Disable global auto-revert mode.
  (global-auto-revert-non-file-buffers 1) ;; Automatically refresh non-file buffers.
  (indent-tabs-mode nil)                  ;; No tabs.
  (ispell-dictionary "en_GB")             ;; Set the default dictionary for spell checking.
  (recentf-mode 1)                        ;; Enable tracking of recently opened files.
  (save-place-mode 1)                     ;; Enable saving the place in files for easier return.
  (savehist-mode 1)                       ;; Enable saving of command history.
  (sentence-end-double-space nil)         ;; Seriously, no one does this anymore.
  (split-width-threshold 300)             ;; Prevent window splitting if the window width exceeds 300 pixels.
  (tab-width 4)                           ;; Set the tab width to 4 spaces.
  (use-short-answers t)                   ;; Use short answers in prompts.
  (winner-mode)                           ;; Enable winner mode to easily undo window config changes.
  (xterm-mouse-mode 1)                    ;; Enable mouse support in terminal mode.

  (modify-coding-system-alist 'file "" 'utf-8)

  ;; Interface enhancements
  (blink-cursor-mode -1)                    ;; Steady cursor.
  (display-line-numbers-type 'relative)     ;; Use relative line numbering in programming modes.
  (display-line-numbers-width 3)            ;; Set a minimum for line numbers width.
  (file-name-shadow-mode 1)                 ;; Enable shadowing of filenames for clarity.
  (global-display-line-numbers-mode 1)      ;; Display line-numbers mode globally.
  (global-hl-line-mode nil)                 ;; Highlight the current line.
  (global-visual-line-mode t)               ;; Visual-Line mode in all buffers.
  (indicate-buffer-boundaries 'left)        ;; Show buffer top and bottom in the margin.
  (show-paren-mode t)                       ;; Highlight matching parens.
  (show-trailing-whitespace nil)            ;; Self-explanatory.
  (switch-to-buffer-obey-display-actions t) ;; Make switching buffers more consistent.
  (x-underline-at-descent-line nil)         ;; Prettier underlines.

  ;; Minibuffer/completion settings.
  (completion-auto-help 'always)                  ; Open completion always; `lazy' another option.
  (completion-auto-select 'second-tab)
  (completion-cycle-threshold 1)                  ; TAB cycles candidates.
  (completion-styles '(basic initials substring)) ; Different styles to match input to candidates.
  (completions-detailed t)                        ; Show annotations.
  (completions-format 'one-column)
  (completions-group t)
  (completions-max-height 20)                     ; This is arbitrary.
  (enable-recursive-minibuffers t)                ; Use the minibuffer whilst in the minibuffer.
  (tab-always-indent 'complete)                   ; When I hit TAB, try to complete, otherwise, indent.

  (keymap-set minibuffer-mode-map "TAB" 'minibuffer-complete) ; TAB acts more like how it does in the shell.

  :config
  ;; Make sure ] b and [ b will always load a file buffer.
  (defun skip-these-buffers (_window buffer _bury-or-kill)
    "Function for `switch-to-prev-buffer-skip'."
    (string-match "\\*[^*]+\\*" (buffer-name buffer)))
    (setq switch-to-prev-buffer-skip 'skip-these-buffers)

  ;; Custom file.
  (setq custom-file (locate-user-emacs-file "custom-vars.el")) ;; Specify the custom file path.
  (load custom-file 'noerror 'nomessage) ;; Load the custom file quietly, ignoring errors.

  ;; Vim brain.
  (global-set-key (kbd "<escape>") 'keyboard-escape-quit)

  ;; Less noise when compiling elisp.
  (setq byte-compile-warnings '(not free-vars unresolved noruntime lexical make-local))
  (setq native-comp-async-report-warnings-errors nil)
  (setq load-prefer-newer t)

  ;; Hide commands in M-x which don't work in the current mode.
  (setq read-extended-command-predicate #'command-completion-default-include-p))

  ;; Disable line numbers in certain contexts.
  (dolist (mode
           '(org-mode-hook
             term-mode-hook
             shell-mode-hook
             eshell-mode-hook
             dashboard-mode-hook
             typst-ts-mode-hook))
    (add-hook mode (lambda () (hl-line-mode 0))))

  ;; Highlight current line in certain modes
  ;(let ((hl-line-hooks '(text-mode-hook prog-mode-hook)))
  ;  (mapc (lambda (hook) (add-hook hook 'hl-line-mode)) hl-line-hooks))

;;; Dired configuration
(use-package dired
  :ensure nil
  :custom
  (dired-listing-switches "-oah --group-directories-first") ;; Display files in a human-readable format and group directories first.
  (dired-dwim-target t) ;; Enable "do what I mean" for target directories.
  (dired-guess-shell-alist-user
   '((".*" "open" "xdg-open"))) ;; Use xdg-open to open everything.
  (dired-kill-when-opening-new-dired-buffer t)) ;; Close the previous buffer when opening a new `dired' instance.

;;; Electric
(use-package electric
  :ensure nil
  :init
  (electric-pair-mode +1) ;; automatically insert closing parens
  (electric-indent-mode -1) ;; weird and inconsistent as hell, go away
  (setq electric-pair-preserve-balance t
        ;; TODO: Why is this here?
        org-edit-src-content-indentation 0))
  ;; Do not auto-pair <> in Org mode, otherwise org-tempo can break.
  (add-hook 'org-mode-hook (lambda ()
             (setq-local electric-pair-inhibit-predicate
                     `(lambda (c)
                    (if (char-equal c ?<) t (,electric-pair-inhibit-predicate c))))))

;;; Emacs window management
(use-package window
  :ensure nil
  :custom
  (display-buffer-alist
   '(
	 ;; ("\\*.*e?shell\\*"
     ;;  (display-buffer-in-side-window)
     ;;  (window-height . 0.25)
     ;;  (side . bottom)
     ;;  (slot . -1))

     ("\\*\\(Backtrace\\|Warnings\\|Compile-Log\\|[Hh]elp\\|Messages\\|Bookmark List\\|Ibuffer\\|Occur\\|eldoc.*\\)\\*"
      (display-buffer-in-side-window)
      (window-height . 0.25)
      (side . bottom)
      (slot . 0))

     ;; Example configuration for the LSP help buffer,
     ;; keeps it always on bottom using 25% of the available space:
     ("\\*\\(lsp-help\\)\\*"
      (display-buffer-in-side-window)
      (window-height . 0.25)
      (side . bottom)
      (slot . 0))

     ;; Configuration for displaying various diagnostic buffers on
     ;; bottom 25%:
     ("\\*\\(Flymake diagnostics\\|Flycheck error messages\\||xref\\|ivy\\|Swiper\\|Completions\\)"
      (display-buffer-in-side-window)
      (window-height . 0.25)
      (side . bottom)
      (slot . 1))
   )))

;;; Org mode
;;
(use-package org
  :ensure nil
  :defer t
  :init
  ;; Edit settings (recommended by org-modern)
  (setq org-auto-align-tags nil
	      org-tags-column 0
	      org-catch-invisible-edits 'show-and-error
	      org-special-ctrl-a/e t ;; special navigation behaviour in headlines
	      org-insert-heading-respect-content t)

  ;;; Return or left-click with mouse follows link
  (customize-set-variable 'org-return-follows-link t)
  (customize-set-variable 'org-mouse-1-follows-link t)

  ;; Styling, hide markup, etc. (recommended by org-modern)
  (setq org-hide-emphasis-markers t
	      org-src-fontify-natively t ;; fontify source blocks natively
	      org-highlight-latex-and-related '(native) ;; fontify latex blocks natively
	      org-pretty-entities t)

  ;; Agenda styling (recommended by org-modern)
  (setq org-agenda-tags-column 0
	      org-agenda-block-separator ?─
	      org-agenda-time-grid
	      '((daily today require-timed)
	        (800 1000 1200 1400 1600 1800 2000)
	        " ┄┄┄┄┄ " "┄┄┄┄┄┄┄┄┄┄┄┄┄┄┄")
	      org-agenda-current-time-string
	      "⭠ now ─────────────────────────────────────────────────")

  (setq org-ellipsis " "))

;; org-tempo
(use-package org-tempo
  :ensure nil
  :after org
  :config
   (dolist (item '(("sh" . "src sh")
                   ("el" . "src emacs-lisp")
                   ("lu" . "src lua")
                   ("py" . "src python")))
   (add-to-list 'org-structure-template-alist item)))

;; toc-org
(use-package toc-org
  :after org
  :commands toc-org-enable
  :init (add-hook 'org-mode-hook 'toc-org-enable))

;; org-bullets
(add-hook 'org-mode-hook 'org-indent-mode)
(use-package org-bullets
  :after org)
(add-hook 'org-mode-hook (lambda () (org-bullets-mode 1)))

;;; Third-party packages

;;; Fonts
;;
;; Define default, variable pitch, and fixed pitch fonts.
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

;; Display commented text and keywords in italics, requires a font with italics support.
(set-face-attribute 'font-lock-comment-face nil
  :slant 'italic)
(set-face-attribute 'font-lock-keyword-face nil
  :slant 'italic)

;; Adjust line spacing.
(setq-default line-spacing 0.15)

;;; Icons
;;
;; nerd-icons
(use-package nerd-icons
  :defer t)

;; nerd-icons-dired
(use-package nerd-icons-dired
  :defer t
  :hook
  (dired-mode . nerd-icons-dired-mode))

;; nerd-icons-completion
(use-package nerd-icons-completion
  :after (:all nerd-icons marginalia)
  :config
  (nerd-icons-completion-mode)
  (add-hook 'marginalia-mode-hook #'nerd-icons-completion-marginalia-setup))

;;; Theme
;;
;; Add local themes to path
(add-to-list 'custom-theme-load-path (concat user-emacs-directory "themes"))

;; doom-themes
(use-package doom-themes
  :config
  (setq doom-themes-enable-bold t
        doom-themes-enable-italic t)
  (load-theme 'doom-gatonegro t)

  (doom-themes-visual-bell-config)
  (doom-themes-org-config))

;; doom-modeline
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

;; dashboard
(use-package dashboard
  :defer t
  :init
  (setq initial-buffer-choice 'dashboard-open
        dashboard-set-heading-icons t
        dashboard-set-file-icons t
        dashboard-banner-logo-title "«Objects such as corpses, painful to view in themselves, can become delightful to contemplate.»"
        ;;dashboard-startup-banner 'logo ;; use standard emacs logo as banner
        dashboard-startup-banner (concat user-emacs-directory "themes/gatonegro.png")
        ;;dashboard-projects-backend 'projectile
        dashboard-center-content t ;; set to 't' for centered content
        dashboard-items '((recents . 5)
                          (agenda . 5 )
                          (bookmarks . 3)
                          ;;(projects . 3)
                          (registers . 3)))
  :custom
  (dashboard-modify-heading-icons '((recents . "file-text")
                                    (bookmarks . "book")))
  :config
  (dashboard-setup-startup-hook))

;;; Completions framework
;;
;; Vertico
(use-package vertico
  :defer t
  :custom
  (vertico-count 10)                    ;; Number of candidates to display in the completion list.
  (vertico-resize nil)                  ;; Disable resizing of the vertico minibuffer.
  (vertico-cycle nil)                   ;; Do not cycle through candidates when reaching the end of the list.
  :config
  ;; Navigate Vertico with C-j and C-k.
  (define-key vertico-map (kbd "C-j") #'vertico-next)
  (define-key vertico-map (kbd "C-k") #'vertico-previous)
  ;; Customize the display of the current candidate in the completion list.
  ;; This will prefix the current candidate with “» ” to make it stand out.
  ;; Reference: https://github.com/minad/vertico/wiki#prefix-current-candidate-with-arrow
  (advice-add #'vertico--format-candidate :around
    (lambda (orig cand prefix suffix index _start)
      (setq cand (funcall orig cand prefix suffix index _start))
      (concat
        (if (= vertico--index index)
          (propertize "» " 'face '(:foreground "#80adf0" :weight bold))
          "  ")
        cand)))
  :hook
   (elpaca-after-init . vertico-mode))

;; Orderless
(use-package orderless
  :defer t                                    ;; Load Orderless on demand.
  :after vertico                              ;; Ensure Vertico is loaded before Orderless.
  :init
  (setq completion-styles '(orderless basic)  ;; Set the completion styles.
        completion-category-defaults nil      ;; Clear default category settings.
        completion-category-overrides '((file (styles partial-completion))))) ;; Customize file completion styles.

;; Marginalia
(use-package marginalia
  :defer t
  :hook
  (elpaca-after-init . marginalia-mode))

;; Company
(use-package company
  :defer t
  :custom
  (company-tooltip-align-annotations t)      ;; Align annotations with completions.
  (company-minimum-prefix-length 1)          ;; Trigger completion after typing 1 character
  (company-idle-delay 0.2)                   ;; Delay before showing completion (adjust as needed)
  (company-tooltip-maximum-width 50)
  :config

  ;; While using C-p C-n to select a completion candidate
  ;; C-y quickly shows help docs for the current candidate
  (define-key company-active-map (kbd "C-y")
			  (lambda ()
				(interactive)
				(company-show-doc-buffer)))
  (define-key company-active-map [tab] 'company-complete-selection)
  (define-key company-active-map [ret] 'company-complete-selection)
  (define-key company-active-map [escape] 'company-abort)
  (define-key company-active-map (kbd "RET") 'company-complete-selection)
  (define-key company-active-map (kbd "C-j") 'company-select-next)
  (define-key company-active-map (kbd "C-k") 'company-select-previous)
  :hook
  (elpaca-after-init . global-company-mode)) ;; Enable Company Mode globally after initialization.

;; Consult
(use-package consult
  :defer t
  :init
  ;; Enhance register preview with thin lines and no mode line.
  (advice-add #'register-preview :override #'consult-register-window)

  ;; Use Consult for xref locations with a preview feature.
  (setq xref-show-xrefs-function #'consult-xref
        xref-show-definitions-function #'consult-xref))

;; Embark
(use-package embark
  :defer t)

;; Embark-Consult
(use-package embark-consult
  :after (:all consult embark)
  :hook
  (embark-collect-mode . consult-preview-at-point-mode)) ;; Enable preview in Embark collect mode.

;;; Dired enhancements
;;
;; * dired-+
(use-package dired-x
  :ensure nil
  :after dired)

;; * dired-open
(use-package dired-open
  :after dired
  :config
  (setq dired-open-extensions '(("gif" . "xdg-open")
                                ("jpg" . "xdg-open")
                                ("png" . "xdg-open")
                                ("mkv" . "xdg-open")
                                ("m4v" . "xdg-open")
                                ("mp4" . "xdg-open"))))

;; * dired-preview
(use-package dired-preview
  :after dired
  :config
     (setq dired-preview-delay 0.7)
     (setq dired-preview-max-size (expt 2 20))
     (setq dired-preview-ignored-extensions-regexp
             (concat "\\."
                     "\\(gz\\|"
                     "zst\\|"
                     "tar\\|"
                     "xz\\|"
                     "rar\\|"
                     "zip\\|"
                     "iso\\|"
                     "epub"
                     "\\)")))

;; Evil - Vim motions
(use-package evil
  :defer t
  :init
  (setq evil-want-integration t
        evil-want-keybinding nil
        evil-vsplit-window-right t
        evil-split-window-below t
        evil-respect-visual-line-mode t
        evil-want-Y-yank-to-eol t
        evil-undo-system 'undo-redo)

  ;; Unbind SPC and TAB so we can use them elsewhere.
  (with-eval-after-load 'evil-maps
    (define-key evil-motion-state-map (kbd "SPC") nil)
    (define-key evil-motion-state-map (kbd "TAB") nil))

  :config
  (evil-set-initial-state 'vterm-mode 'emacs)

  :hook
  (elpaca-after-init . evil-mode))

;; Evil-Collection
(use-package evil-collection
  :defer t

  :custom
  (evil-collection-want-find-usages-bindings t)

  :hook
  (evil-mode . evil-collection-init))

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

;; Dired
(gato/leader-keys
  "d" '(:ignore t :wk "Dired")
  "d d" '(dired :wk "Open Dired")
  "d j" '(dired-jump :wk "Dired jump to current")
  "d w" '(:ignore t :wk "Writable Dired")
  "d w w" '(wdired-change-to-wdired-mode :wk "Enable writable Dired")
  "d w a" '(wdired-abort-changes :wk "Abort writable Dired changes")
  "d w f" '(wdired-finish-edit :wk "Finish writable Dired edit"))

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
  "* /" 'dired-mark-directories)

;; Evaluate Elisp
(gato/leader-keys
  "e" '(:ignore t :wk "Evaluate")
  ;; Evaluate
  "e b" '(eval-buffer :wk "Evaluate Elisp in buffer")
  "e d" '(eval-defun :wk "Evaluate defun containing or after point")
  "e e" '(eval-expression :wk "Evaluate Elisp expression")
  "e l" '(eval-last-sexp :wk "Evaluate Elisp expression before point")
  "e r" '(eval-region :wk "Evaluate Elisp in region"))

;; Find files
(gato/leader-keys
  "." '(find-file :wk "Find file")
  "f" '(:ignore t :wk "Find")
  "f c" '((lambda () (interactive) (find-file "~/.config/emacs/README.org")) :wk "Emacs configuration")
  "f r" '(recentf :wk "Recent files")
  "f u" '(sudo-edit-find-file :wk "Sudo find file")
  "f U" '(sudo-edit :wk "Sudo edit this file"))

;; Help functions
(gato/leader-keys
 "h" '(:ignore t :wk "Help")
 "h f" '(apropos-command :wk "Apropos command")
 "h f" '(describe-function :wk "Describe function")
 "h k" '(describe-key :wk "Describe key")
 "h m" '(describe-mode :wk "Describe mode")
 "h v" '(describe-variable :wk "Describe variable")
 "h x" '(describe-command :wk "Describe command")
 "h t" '(load-theme :wk "Load theme")
 "h r" '((lambda () (interactive) (load-file user-init-file)) :wk "Reload Emacs config"))

;; Toggle
(gato/leader-keys
  "t" '(:ignore t :wk "Toggle")
  ;;"t c" '(visual-line-fill-column-mode :wk "Toggle fill column")
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
  "w w" '(evil-window-next :wk "Goto next window"))
  ;; Move windows
  ;;"w H" '(buf-move-left :wk "Buffer move left")
  ;;"w J" '(buf-move-down :wk "Buffer move down")
  ;;"w K" '(buf-move-up :wk "Buffer move up")
  ;;"w L" '(buf-move-right :wk "Buffer move right"))

) ;; end of general.el keybindings

;; Rainbow delimiters
(use-package rainbow-delimiters
  :defer t
  :ensure t
  :hook org-mode prog-mode)

;; Rainbow mode
(use-package rainbow-mode
  :defer t
  :diminish
  :hook org-mode prog-mode)

;; Sudo-edit
(use-package sudo-edit
  :defer t)

;; Which-Key - So many keys
(use-package which-key
  :defer t

  :config
  (setq which-key-side-window-location 'bottom
        which-key-sort-order #'which-key-key-order-alpha
        which-key-sort-uppercase-first nil
        which-key-add-column-padding 1
        which-key-max-display-columns nil
        which-key-min-display-lines 6
        which-key-side-window-slot -10
        which-key-side-window-max-height 0.25
        which-key-idle-delay 0.5
        which-key-max-description-length nil
        which-key-allow-imprecise-window-fit nil
        which-key-separator "  ")

  :hook
  (elpaca-after-init . which-key-mode))

;; Show the help buffer after startup
;;(add-hook 'elpaca-after-init-hook 'help-quick)

(provide 'init)

;;; init.el ends here
