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

  ;; Display a counter showing the number of the current and the other
  ;; matches.  Place it before the prompt, though it can be after it.
  (setq isearch-lazy-count t)
  (setq lazy-count-prefix-format "(%s/%s) ")
  (setq lazy-count-suffix-format nil)

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

  ;; Hide commands in M-x which don't work in the current mode.
  (setq read-extended-command-predicate #'command-completion-default-include-p))

  ;; Disable line numbers and hl-line mode in certain contexts.
  (dolist (mode
           '(dashboard-mode-hook
             eshell-mode-hook
             markdown-mode-hook
             nov-mode-hook
             org-mode-hook
             shell-mode-hook
             term-mode-hook
             typst-ts-mode-hook))
    (add-hook mode (lambda () (hl-line-mode 0)))
    (add-hook mode (lambda () (display-line-numbers-mode 0))))

;;; Dired configuration
(use-package dired
  :ensure nil
  :config
  (setq dired-listing-switches "-AGFhlv --group-directories-first --time-style=long-iso"
        dired-dwim-target t
        dired-guess-shell-alist-user '((".*" "xdg-open"))
        dired-kill-when-opening-new-dired-buffer t
        dired-auto-revert-buffer #'dired-directory-changed-p
        dired-make-directory-clickable t
        dired-mouse-drag-files t)

  (add-hook 'dired-mode-hook #'dired-hide-details-mode)
  (add-hook 'dired-mode-hook #'hl-line-mode))

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

(use-package project
  :ensure nil
  :defer t)

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
  ;; Edit settings
  (setq org-auto-align-tags nil
	    org-tags-column 0
	    org-catch-invisible-edits 'show-and-error
	    org-special-ctrl-a/e t ;; special navigation behaviour in headlines
	    org-insert-heading-respect-content t

        ;; Styling, hide markup, etc.
        org-startup-indented nil
        org-pretty-entities t
        org-use-sub-superscripts "{}"
        org-hide-emphasis-markers t
        org-startup-with-inline-images t
        org-image-actual-width '(300)
        org-src-fontify-natively t
        org-highlight-latex-and-related '(native)

        ;; Agenda styling
        org-agenda-tags-column 0
	    org-agenda-block-separator ?─
	    org-agenda-time-grid
	    '((daily today require-timed)
	      (800 1000 1200 1400 1600 1800 2000)
	      " ┄┄┄┄┄ " "┄┄┄┄┄┄┄┄┄┄┄┄┄┄┄")
	    org-agenda-current-time-string
	    "⭠ now ─────────────────────────────────────────────────")

  (setq org-ellipsis " ")

  ;;; Return or left-click with mouse follows link
  (customize-set-variable 'org-return-follows-link t)
  (customize-set-variable 'org-mouse-1-follows-link t))

(use-package org-appear
  :defer t
  :after org
  :config
  (setq org-appear-autoemphasis t
        org-appear-autosubmarkers t
        org-appear-autolinks t)
  :hook
  (org-mode . org-appear-mode))

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

;;; org-modern
(use-package org-modern
  :defer t
  :hook
  (org-mode . global-org-modern-mode)
  :custom
  (org-modern-keyword nil)
  (org-modern-checkbox nil)
  (org-modern-table nil)
  (org-modern-star 'replace))

;; org-tree-slide
(use-package org-tree-slide
  :defer t)

;;; Third-party packages

;;; Fonts
;;
;; Define default, variable pitch, and fixed pitch fonts.
(set-face-attribute 'default nil
  :family "monospace"
  :height 90)
(set-face-attribute 'variable-pitch nil
  :family "ETbb"
  :height 1.5
  :weight 'medium)
(set-face-attribute 'fixed-pitch nil
  :family "monospace"
  :height 1.0)

;; Display commented text and keywords in italics, requires a font with italics support.
(set-face-attribute 'font-lock-comment-face nil
  :slant 'italic)
(set-face-attribute 'font-lock-keyword-face nil
  :slant 'italic)

;; Adjust line spacing.
(setq-default line-spacing 0.35)

;; Org mode title heights
;; (custom-set-faces
;;   '(org-level-1 ((t (:inherit outline-1 :height 1.5))))
;;   '(org-level-2 ((t (:inherit outline-2 :height 1.25))))
;;   '(org-level-3 ((t (:inherit outline-3 :height 1.1))))
;;   '(org-level-4 ((t (:inherit outline-4 :height 1.0))))
;;   '(org-level-5 ((t (:inherit outline-5 :height 1.0))))
;; )

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
;; NOTE: Install `extra/ttf-nerd-fonts-symbols-mono', otherwise icon spacing is broken
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
        dashboard-icon-type 'nerd-icons
        dashboard-banner-logo-title "«Objects such as corpses, painful to view in themselves, can become delightful to contemplate.»"
        ;;dashboard-startup-banner 'logo ;; use standard emacs logo as banner
        dashboard-startup-banner (concat user-emacs-directory "themes/gatonegro.png")
        dashboard-projects-backend 'project-el
        dashboard-center-content t ;; set to 't' for centered content
        dashboard-items '((recents . 5)
                          (agenda . 5 )
                          (bookmarks . 3)
                          (projects . 3)
                          (registers . 3)))
  :config
  (dashboard-setup-startup-hook))

(use-package spacious-padding
  :defer t
  :config
    (setq spacious-padding-widths
      '( :internal-border-width 20
         :header-line-width 4
         :mode-line-width 0
         :tab-width 4
         :right-divider-width 15
         :scroll-bar-width 4))
  :hook
   (elpaca-after-init . spacious-padding-mode))

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
        evil-undo-system 'undo-tree)

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

;; Consult
(gato/leader-keys
  "c" '(:ignore t :wk "Consult")
  "c f" '(consult-fd :wk "Find files with 'fd' in DIR")
  "c g" '(consult-grep :wk "Find files with 'grep' in DIR")
  "c h" '(consult-outline :wk "Jump to outline heading")
  "c l" '(consult-line :wk "Find matching line")
  "c o" '(consult-org-heading :wk "Jump to Org heading"))

(general-define-key
  :states 'normal
  "] c" '(diff-hl-next-hunk :wk "Next diff hunk")
  "[ c" '(diff-hl-previous-hunk :wk "Previous diff hunk"))

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
  "o" 'dired-open-xdg
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

;; Extended command
(gato/leader-keys
  "SPC" '(execute-extended-command :wk "Execute extended command"))

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
 "h t" '(consult-theme :wk "Consult-theme")
 "h r" '((lambda () (interactive) (load-file user-init-file)) :wk "Reload Emacs config"))

;; Magit
(gato/leader-keys
 "g" '(:ignore t :wk "Magit")
 "g s" '(magit-status :wk "Status"))

(general-define-key
  :states 'normal
  :keymaps 'nov-mode-map
  "J" 'nov-scroll-up
  "K" 'nov-scroll-down
  "t" 'nov-goto-toc
  "H" 'nov-previous-document
  "L" 'nov-next-document)

(general-define-key
  :states 'normal
  :keymaps 'pdf-view-mode-map
  "j" 'pdf-view-next-line-or-next-page
  "k" 'pdf-view-previous-line-or-previous-page
  "r" 'pdf-view-themed-minor-mode
  "g g" 'pdf-view-first-page
  "G" 'pdf-view-last-page
  "g p" 'pdf-view-goto-page
  "=" 'pdf-view-enlarge
  "-" 'pdf-view-shrink)

;; Toggle
  (gato/leader-keys
    "t" '(:ignore t :wk "Toggle")
    "t h" '(hl-line-mode :wk "Line highlight")
    "t l" '(display-line-numbers-mode :wk "Line numbers")
    "t o" '(olivetti-mode :wk "Olivetti")
    "t t" '(visual-line-mode :wk "Truncated lines"))
    ;; "t v" '(vterm-toggle :wk "Vterm"))

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

(gato/leader-keys
  :keymaps 'typst-ts-mode-map
  "p" '(:ignore t :wk "Typst")
  "p c" '(typst-ts-compile :wk "Compile")
  "p w" '(typst-ts-watch-mode :wk "Watch")
  "p o" '(typst-ts-mode-preview :wk "Open compiled document")
  "p p" '(typst-ts-compile-and-preview :wk "Compile and preview"))

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

;; Diff-hl
(use-package diff-hl
  :defer t
  :hook
  (find-file . (lambda ()
               (global-diff-hl-mode)   ;; Enable Diff-HL mode for all files.
               (diff-hl-flydiff-mode)  ;; Automatically refresh diffs.
               (diff-hl-margin-mode))) ;; Show diff indicators in the margin.
  :custom
  (diff-hl-side 'left)                           ;; Set the side for diff indicators.
  (diff-hl-margin-symbols-alist '((insert . "│") ;; Customize symbols for each change type.
                                  (delete . "-")
                                  (change . "│")
                                  (unknown . "?")
                                  (ignored . "i"))))

;; Transient - Install updated version needed by Magit
(use-package transient
  :defer t)

;; Magit - Git client for Emacs
(use-package magit
  :after transient
  :defer t)

(use-package nov
  :defer t

  :init
  (add-to-list 'auto-mode-alist '("\\.epub\\'" . nov-mode))

  :config
  (setq nov-text-width t)
  (add-hook 'nov-mode-hook 'olivetti-mode))

(use-package olivetti
  :defer t

  :config
  (setq olivetti-body-width 82))

(use-package pdf-tools
  :defer t
  :ensure (:type git :host github :repo "aikrahguzar/pdf-tools"
                 :branch "upstream-pdf-roll")
  :commands (pdf-loader-installer)
  :mode "\\.pdf\\'"
  :init (pdf-loader-install)
  :config
    (add-to-list 'revert-without-query ".pdf")
     )

(add-hook 'pdf-view-mode-hook #'(lambda () (interactive) (display-line-numbers-mode -1)
                                                         (pdf-view-themed-minor-mode t)
                                                         (pdf-view-roll-minor-mode)))

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

(use-package undo-tree
  :defer t
  :hook
  (elpaca-after-init . global-undo-tree-mode)
  :init
  (setq undo-tree-visualizer-timestamps t
        undo-tree-visualizer-diff t
        ;; Increase undo limits to avoid losing history due to Emacs' garbage collection.
        undo-limit 800000                     ;; Limit for undo entries.
        undo-strong-limit 12000000            ;; Strong limit for undo entries.
        undo-outer-limit 120000000)           ;; Outer limit for undo entries.
  :config
  (setq undo-tree-history-directory-alist '(("." . "~/.config/emacs/.cache/undo"))))

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

;; Flycheck
(use-package flycheck
  :defer t
  :init (global-flycheck-mode))

;; Treesit-auto
(use-package treesit-auto
  :after emacs
  :custom
  (treesit-auto-install 'prompt)
  :config
  (treesit-auto-add-to-auto-mode-alist 'all)
  (global-treesit-auto-mode t))

;;; LSP-mode
(use-package lsp-mode
  :defer t
  :hook (
         (bash-ts-mode . lsp)  ;; Bash
         (lua-mode . lsp)      ;; Lua
         (python-mode . lsp)   ;; Python
         (typst-ts-mode . lsp) ;; Typst
         (lsp-mode . lsp-enable-which-key-integration)) ;; Integrate with Which Key
  :commands lsp
  :custom
  (lsp-keymap-prefix "C-c l")                           ;; Set the prefix for LSP commands.
  (lsp-inlay-hint-enable t)                             ;; Enable inlay hints.
  (lsp-completion-provider :none)                       ;; Disable the default completion provider.
  (lsp-session-file (locate-user-emacs-file ".lsp-session")) ;; Specify session file location.
  (lsp-log-io nil)                                      ;; Disable IO logging for speed.
  (lsp-idle-delay 0)                                    ;; Set the delay for LSP to 0 (debouncing).
  (lsp-keep-workspace-alive nil)                        ;; Disable keeping the workspace alive.
  ;; Core settings
  (lsp-enable-xref t)                                   ;; Enable cross-references.
  (lsp-auto-configure t)                                ;; Automatically configure LSP.
  (lsp-enable-links nil)                                ;; Disable links.
  (lsp-eldoc-enable-hover t)                            ;; Enable ElDoc hover.
  (lsp-enable-file-watchers nil)                        ;; Disable file watchers.
  (lsp-enable-folding nil)                              ;; Disable folding.
  (lsp-enable-imenu t)                                  ;; Enable Imenu support.
  (lsp-enable-indentation nil)                          ;; Disable indentation.
  (lsp-enable-on-type-formatting nil)                   ;; Disable on-type formatting.
  (lsp-enable-suggest-server-download t)                ;; Enable server download suggestion.
  (lsp-enable-symbol-highlighting t)                    ;; Enable symbol highlighting.
  (lsp-enable-text-document-color nil)                  ;; Disable text document color.
  ;; Modeline settings
  (lsp-modeline-code-actions-enable nil)                ;; Keep modeline clean.
  (lsp-modeline-diagnostics-enable nil)                 ;; Use `flycheck' instead.
  (lsp-modeline-workspace-status-enable t)              ;; Display "LSP" in the modeline when enabled.
  (lsp-signature-doc-lines 1)                           ;; Limit echo area to one line.
  (lsp-eldoc-render-all nil)                            ;; Render all ElDoc messages.
  ;; Completion settings
  (lsp-completion-enable t)                             ;; Enable completion.
  (lsp-completion-enable-additional-text-edit t)        ;; Enable additional text edits for completions.
  (lsp-enable-snippet nil)                              ;; Disable snippets
  (lsp-completion-show-kind t)                          ;; Show kind in completions.
  ;; Headerline settings
  (lsp-headerline-breadcrumb-enable nil)                ;; Enable symbol numbers in the headerline.
  (lsp-headerline-breadcrumb-enable-symbol-numbers nil) ;; Enable symbol numbers in the headerline.
  (lsp-headerline-arrow "▶")                            ;; Set arrow for headerline.
  (lsp-headerline-breadcrumb-enable-diagnostics nil)    ;; Disable diagnostics in headerline.
  (lsp-headerline-breadcrumb-icons-enable nil)          ;; Disable icons in breadcrumb.
  ;; Semantic settings
  (lsp-semantic-tokens-enable nil))                     ;; Disable semantic tokens.

;; Markdown
(use-package markdown-mode
  :defer t
  :init (setq markdown-command "multimarkdown"))

;; Lua
(use-package lua-mode
  :defer t)

;; Typst support
(use-package typst-ts-mode
  :defer t
  :ensure (:type git :host codeberg :repo "meow_king/typst-ts-mode"
                 :files (:defaults "*.el"))
  :custom
  (typst-ts-watch-options "--open")
  (typst-ts-mode-grammar-location (expand-file-name "tree-sitter/libtree-sitter-typst.so" user-emacs-directory))
  (typst-ts-mode-enable-raw-blocks-highlight t)

  ;; Register `tinymist' as the Typst language server
  :config
  (with-eval-after-load 'lsp-mode
    (add-to-list 'lsp-language-id-configuration
                 '(typst-ts-mode . "typst"))

    (lsp-register-client
     (make-lsp-client :new-connection (lsp-stdio-connection "tinymist")
                      :activation-fn (lsp-activate-on "typst")
                      :server-id 'tinymist))))

(provide 'init)

;;; init.el ends here
