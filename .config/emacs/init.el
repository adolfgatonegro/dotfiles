;;; Package management
;; Clone, build, and initialise Elpaca.
(defvar elpaca-installer-version 0.8)
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
        (if-let* ((buffer (pop-to-buffer-same-window "*elpaca-bootstrap*"))
                  ((zerop (apply #'call-process `("git" nil ,buffer t "clone"
                                                  ,@(when-let* ((depth (plist-get order :depth)))
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

(add-to-list 'load-path
             (expand-file-name "lisp" user-emacs-directory))
(add-to-list 'custom-theme-load-path
             (expand-file-name "themes" user-emacs-directory))

(defun gato/smart-kill-or-delete (&optional arg)
  "If Emacs is running as daemon, delete the current frame,
otherwise, prompt to save buffers and exit completely."
  (interactive "p")
  (if (not (daemonp))
      (save-buffers-kill-emacs)
    (save-some-buffers (eq arg 4))
    (mapc 'delete-frame (frames-on-display-list))))

;;; Emacs essentials
(use-package emacs
  :ensure nil
  :demand t

  :config
  ;; Disable backups and lockfiles.
  (setq make-backup-files nil)
  (setq backup-inhibited t)
  (setq create-lockfiles nil)

  ;; Disable the custom file.
  (setq custom-file (make-temp-file "emacs-custom-"))

  ;; Vim brain is real.
  (global-set-key (kbd "<escape>") 'keyboard-escape-quit)

  ;; Set UTF-8 encoding everywhere.
  (set-charset-priority 'unicode)
  (setq locale-coding-system 'utf-8
        coding-system-for-read 'utf-8
        coding-system-for-write 'utf-8)
  (set-terminal-coding-system 'utf-8)
  (set-keyboard-coding-system 'utf-8)
  (set-selection-coding-system 'utf-8)
  (prefer-coding-system 'utf-8)
  (setq default-process-coding-system '(utf-8-unix . utf-8-unix))

  ;; Always start with the *scratch* buffer.
  (setq initial-buffer-choice t)
  (setq initial-major-mode 'lisp-interaction-mode)
  (setq initial-scratch-message
        (format ";; This is `%s'.  Use `%s' to evaluate and print results.\n\n"
                'lisp-interaction-mode
                (propertize
                 (substitute-command-keys "\\<lisp-interaction-mode-map>\\[eval-print-last-sexp]")
                 'face 'help-key-binding)))

  ;; Declare all themes as safe
  (setq custom-safe-themes t)

  ;; Hide commands in M-x which don't work in the current mode.
  (setq read-extended-command-predicate #'command-completion-default-include-p)

  ;; Steady cursor mode.
  (blink-cursor-mode -1)

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

  ;; Display a counter showing the number of the current and the other
  ;; matches.  Place it before the prompt, though it can be after it.
  (setq isearch-lazy-count t)
  (setq lazy-count-prefix-format "(%s/%s) ")
  (setq lazy-count-suffix-format nil)

  :custom
  ;; `emacs-kick' better defaults, review in progress.
  (column-number-mode t)                  ;; Display the column number in the mode line.
  (delete-by-moving-to-trash t)           ;; Move deleted files to the trash.
  (indent-tabs-mode nil)                  ;; No tabs.
  (ispell-dictionary "en_GB")             ;; Set the default dictionary for spell checking.
  (save-place-mode 1)                     ;; Enable saving the place in files for easier return.
  (savehist-mode 1)                       ;; Enable saving of command history.
  (sentence-end-double-space nil)         ;; Seriously, no one does this anymore.
  (split-height-threshold 80)             ;; Prevent window splitting if the window height exceeds 80 pixels.
  (split-width-threshold 125)             ;; Prevent window splitting if the window width exceeds 125 pixels.
  (tab-width 4)                           ;; Set the tab width to 4 spaces.
  (winner-mode)                           ;; Enable winner mode to easily undo window config changes.
  (xterm-mouse-mode 1)                    ;; Enable mouse support in terminal mode.

  (modify-coding-system-alist 'file "" 'utf-8)

  ;; Interface enhancements
  (display-line-numbers-type 'relative)     ;; Use relative line numbering in programming modes.
  (display-line-numbers-width 3)            ;; Set a minimum for line numbers width.
  (file-name-shadow-mode 1)                 ;; Enable shadowing of filenames for clarity.
  (global-display-line-numbers-mode 1)      ;; Display line-numbers mode globally.
  (global-hl-line-mode nil)                 ;; Highlight the current line.
  (global-visual-line-mode t)               ;; Visual-Line mode in all buffers.
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

  :bind (:map global-map
         ("C-x C-c" . 'gato/smart-kill-or-delete)
         ("C-x k" . kill-current-buffer)
         ("C-z" . nil)
         ("C-x C-z" . nil)
         ("C-h h" . nil)
         ("M-`" . duplicate-line)
         ("M-=" . count-words)
         ("M-c" . capitalize-dwim)
         ("M-l" . downcase-dwim)
         ("M-u" . upcase-dwim)
         ("M-z" . zap-up-to-char))
  ) ;; End of `use-package'

;;;; Auto revert mode
(use-package autorevert
  :ensure nil
  :hook (elpaca-after-init . global-auto-revert-mode)
  :config
  (setq auto-revert-verbose t))

;;;; Built-in bookmarking framework (bookmark.el)
(use-package bookmark
  :ensure nil
  :commands (bookmark-set bookmark-jump bookmark-bmenu-list)
  :hook (bookmark-bmenu-mode . hl-line-mode)
  :config
  (setq bookmark-use-annotations nil)
  (setq bookmark-automatically-show-annotations nil)
  (setq bookmark-fringe-mark nil) ; Emacs 29 to hide bookmark fringe icon
  ;; Write changes to the bookmark file as soon as 1 modification is
  ;; made (addition or deletion).  Otherwise Emacs will only save the
  ;; bookmarks when it closes, which may never happen properly
  ;; (e.g. power failure).
  (setq bookmark-save-flag 1))

;;; Electric
(use-package electric
  :ensure nil
  :init
  (electric-pair-mode +1) ;; automatically insert closing parens
  (electric-indent-mode -1) ;; weird and inconsistent as hell, go away
  (setq electric-pair-preserve-balance t)
  ;; Do not auto-pair <> in Org mode, otherwise org-tempo can break.
  (add-hook 'org-mode-hook (lambda ()
             (setq-local electric-pair-inhibit-predicate
                     `(lambda (c)
                    (if (char-equal c ?<) t (,electric-pair-inhibit-predicate c)))))))

;;;; Delete selection
(use-package delsel
  :ensure nil
  :hook (after-init . delete-selection-mode))

;; Keeping track of recently visited files.
(use-package recentf
  :ensure nil
  :hook (elpaca-after-init . recentf-mode)
  :config
  (setq recentf-max-saved-items 100)
  (setq recentf-max-menu-items 25)
  (setq recentf-save-file-modes nil)
  ;; (setq recentf-keep nil)
  (setq recentf-auto-cleanup nil)
  (setq recentf-initialize-file-name-history nil)
  (setq recentf-filename-handlers nil)
  (setq recentf-show-file-shortcuts-flag nil))

;;;; Repeatable key chords (repeat-mode)
(use-package repeat
  :ensure nil
  :hook (elpaca-after-init . repeat-mode)
  :config
  (setq repeat-on-final-keystroke t
        repeat-exit-timeout 5
        repeat-exit-key "<escape>"
        repeat-keep-prefix nil
        repeat-check-key t
        repeat-echo-function 'ignore
        ;; Technically, this is not in repeal.el, though it is the
        ;; same idea.
        set-mark-command-repeat-pop t))

;;; Directional window motions (windmove)
(use-package windmove
  :ensure nil
  :bind
  ;; Those override some commands that are already available with
  ;; C-M-u, C-M-f, C-M-b.
  (("C-M-<up>" . windmove-up)
   ("C-M-<right>" . windmove-right)
   ("C-M-<down>" . windmove-down)
   ("C-M-<left>" . windmove-left)
   ("C-M-S-<up>" . windmove-swap-states-up)
   ("C-M-S-<right>" . windmove-swap-states-right) ; conflicts with `org-increase-number-at-point'
   ("C-M-S-<down>" . windmove-swap-states-down)
   ("C-M-S-<left>" . windmove-swap-states-left)))

;;; Dired configuration
(use-package dired
  :ensure nil
  :commands (dired)
  :defines dired-mode-map
  :config
  (setq dired-listing-switches "-AGFhlv --group-directories-first --time-style=long-iso"
        dired-dwim-target t
        dired-guess-shell-alist-user '((".*" "xdg-open"))
        dired-kill-when-opening-new-dired-buffer t
        dired-auto-revert-buffer #'dired-directory-changed-p
        dired-recursive-copies 'always
        dired-recursive-deletes 'always
        dired-make-directory-clickable t
        dired-mouse-drag-files t
        delete-by-moving-to-trash t)
  :init
  (add-hook 'dired-mode-hook
            (lambda ()
              (dired-hide-details-mode)
              (hl-line-mode)
              (auto-revert-mode)
              (setq-default auto-revert-interval 1)
              (auto-revert-set-timer)))
  :bind (:map dired-mode-map
              ("<left>" . dired-up-directory)
              ("C-+" . dired-create-empty-file)))

;;; Dired enhancements
;;
;; dired-aux
(use-package dired-aux
  :ensure nil
  :after dired
  :config
  (setq dired-isearch-filenames 'dwim)
  (setq dired-create-destination-dirs 'ask)
  (setq dired-vc-rename-file t)
  (setq dired-do-revert-buffer (lambda (dir) (not (file-remote-p dir))))
  (setq dired-create-destination-dirs-on-trailing-dirsep t))

;; dired-x
(use-package dired-x
  :ensure nil
  :after dired
  :bind (:map dired-mode-map
              ("I" . dired-info))
  :config
  (setq dired-clean-up-buffers-too t)
  (setq dired-clean-confirm-killing-deleted-buffers t)
  (setq dired-x-hands-off-my-keys t))

;; dired-open
(use-package dired-open
  :after dired
  :config
  (setq dired-open-extensions '(("gif" . "xdg-open")
                                ("jpg" . "xdg-open")
                                ("png" . "xdg-open")
                                ("mkv" . "xdg-open")
                                ("m4v" . "xdg-open")
                                ("mp4" . "xdg-open")))
  :bind (:map dired-mode-map
              ("<right>" . dired-open-file)
              ("C-<return>" . dired-open-xdg)))

;; dired-preview
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
                     "\\)"))
 :bind (:map dired-mode-map
             ("p" . dired-preview-mode)))

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

(use-package evil
  :hook (elpaca-after-init . evil-mode)
  :custom
  (evil-undo-system 'undo-redo)
  ;; use Emacs bindings in insert-mode
  (evil-disable-insert-state-bindings t)
  (evil-want-keybindings nil)
  :config
  (evil-respect-visual-line-mode t)
  (evil-want-Y-yank-to-eol t)
  ;; define modes which should start in Emacs state
  (evil-set-initial-state 'pdf-view-mode 'emacs)

  ;; define custom bindings
  (evil-define-key 'normal dired-mode-map "h" 'dired-up-directory)
  (evil-define-key 'normal dired-mode-map "j" 'dired-next-line)
  (evil-define-key 'normal dired-mode-map "k" 'dired-previous-line)
  (evil-define-key 'normal dired-mode-map "l" 'dired-open-file))

;; limit `evil' outside actual text navigation and editing
(with-eval-after-load 'evil-maps
  (define-key evil-motion-state-map (kbd "SPC") nil)
  (define-key evil-motion-state-map (kbd "RET") nil)
  (define-key evil-normal-state-map (kbd "<escape>") 'keyboard-escape-quit)
  (define-key evil-insert-state-map (kbd "<escape>") 'evil-normal-state)
  (define-key evil-motion-state-map (kbd "TAB") nil))

;;; Fonts
;;
;; Define default, variable pitch, and fixed pitch fonts.
(set-face-attribute 'default nil
  :family "monospace"
  :height 100)
(set-face-attribute 'variable-pitch nil
  :family "ETbb"
  :height 1.25)
(set-face-attribute 'fixed-pitch nil
  :family "monospace"
  :height 1.0)

;; Display commented text and keywords in italics, requires a font with italics support.
(set-face-attribute 'font-lock-comment-face nil
  :slant 'italic)
(set-face-attribute 'font-lock-keyword-face nil
  :slant 'italic)

;; Adjust line spacing.
(setq-default line-spacing 0.4)

;; Org mode title heights
;(custom-set-faces
;  '(org-document-title ((t (:inherit default :height 1.2)))))
;  '(org-level-1 ((t (:inherit outline-1 :height 1.2))))
;  '(org-level-2 ((t (:inherit outline-2 :height 1.1))))
;  '(org-level-3 ((t (:inherit outline-3 :height 1.05))))
;  '(org-level-4 ((t (:inherit outline-4 :height 1.0))))
;  '(org-level-5 ((t (:inherit outline-5 :height 1.0))))
;  '(org-level-6 ((t (:inherit outline-5 :height 1.0))))
;  '(org-level-7 ((t (:inherit outline-5 :height 1.0))))
;  '(org-level-8 ((t (:inherit outline-5 :height 1.0)))))

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

(use-package spacious-padding
  :defer t
  :config
  (setq spacious-padding-widths
        '( :internal-border-width 25
           :header-line-width 4
           :mode-line-width 6
           :tab-width 4
           :right-divider-width 30
           :scroll-bar-width 8
           :fringe-width 8))

  (setq spacious-padding-subtle-mode-line
        `( :mode-line-active 'default
           :mode-line-inactive vertical-border))
  :hook
   (elpaca-after-init . spacious-padding-mode))

(use-package ef-themes
    :init
    (setq ef-themes-mixed-fonts t
          ef-themes-variable-pitch-ui t)
    (setq ef-themes-headings
          '((0 regular variable-pitch 1.2)
            (1 light variable-pitch 1.5)
            (2 light variable-pitch 1.4)
            (3 light variable-pitch 1.3)
            (4 light variable-pitch 1.2)
            (t light variable-pitech)))
    (setq ef-winter-palette-overrides
          '((bg-main "#0b0b15")
            (fg-main "#c6c6d5")
            (bg-dim "#1d1d2f")
            (fg-dim "#80809f")
            (bg-alt "#2f2f42")
            (fg-alt "#bfbfef")
            (bg-active "#4a4a62")
            (bg-inactive "#19191f")))  
    (mapc #'disable-theme custom-enabled-themes)
    :config
    (load-theme 'ef-winter :no-confirm))
;;  Override mode-line colours
;;    (with-eval-after-load 'ef-themes
;;      (ef-themes-with-colors
;;        (set-face-attribute 'header-line nil
;;                            :background bg-main
;;                            :foreground fg-main
;;                            :box 'unspecified)
;;        (set-face-attribute 'mode-line nil
;;                            :background bg-main
;;                            :foreground fg-main
;;                            :box 'unspecified)
;;        (set-face-attribute 'mode-line-inactive nil
;;                            :background bg-dim
;;                            :foreground fg-dim
;;                            :box 'unspecified)))

;;; Mode line
(use-package prot-modeline
  :ensure nil
  :config
  (setq mode-line-compact t) ; Emacs 28
  (setq mode-line-right-align-edge 'right-margin) ; Emacs 30
  ;;(setq-default mode-line-format 'nil)
  (setq-default mode-line-format
                '("%e"
                  prot-modeline-kbd-macro
                  prot-modeline-narrow
                  prot-modeline-buffer-status
                  prot-modeline-window-dedicated-status
                  prot-modeline-input-method
                  "  "
                  prot-modeline-buffer-identification
                  "  "
                  prot-modeline-major-mode
                  prot-modeline-process
                  "  "
                  prot-modeline-vc-branch
                  "  "
                  prot-modeline-eglot
                  "  "
                  prot-modeline-flymake
                  "  "
                  mode-line-format-right-align ; Emacs 30
                  ;;prot-modeline-notmuch-indicator
                  "  "
                  prot-modeline-misc-info))

        (set-face-attribute 'prot-modeline-indicator-button nil :box 'unspecified))

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
        xref-show-definitions-function #'consult-xref)
  :bind (:map global-map
         ("M-s M-b" . consult-buffer)
         ("M-s M-m" . consult-bookmark)
         ("M-s M-f" . consult-find)
         ("M-s M-g" . consult-grep)
         ("M-s M-l" . consult-line)
         ("M-s M-s" . consult-outline)
         ("M-g M-g" . consult-goto-line)))

;; Embark
(use-package embark
  :defer t)

;; Embark-Consult
(use-package embark-consult
  :after (:all consult embark)
  :hook
  (embark-collect-mode . consult-preview-at-point-mode)) ;; Enable preview in Embark collect mode.

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

(use-package jinx
  :defer t
  :bind (("M-$" . jinx-correct)
         ("C-M-$" . jinx-languages))
  :config
  (setq jinx-languages "en_GB es fr it")
  :hook
  (elpaca-after-init . global-jinx-mode))

(use-package olivetti
  :defer t
  :config
  (setq olivetti-body-width 82)
  :bind (:map global-map
              ("C-c t o" . olivetti-mode)))

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

(use-package yasnippet
  :defer t
  :config
  (setq yas-snippet-dirs '("~/.config/emacs/snippets"))
  :hook
  (elpaca-after-init . yas-global-mode))

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

  (setq org-ellipsis "")

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

;;; org-modern
(use-package org-modern
  :defer t
  :config
  (setq org-modern-fold-stars '(("◉" . "○")
                                ("●" . "○")
                                ("●" . "○")
                                ("●" . "○")
                                ("●" . "○")))
  :custom
  (org-modern-hide-stars 'leading)
  (org-modern-keyword t)
  (org-modern-checkbox nil)
  (org-modern-table nil)
  (org-modern-star 'fold)

  :hook
  (org-mode . global-org-modern-mode))

;; org-tree-slide
(use-package org-tree-slide
  :defer t)

(use-package eglot
  :ensure nil
  :config
  (add-to-list 'eglot-server-programs '((typst-ts-mode) "tinymist"))
  :hook
  (typst-ts-mode . eglot-ensure))

;; Treesit-auto
(use-package treesit-auto
  :after emacs
  :custom
  (treesit-auto-install 'prompt)
  :config
  (treesit-auto-add-to-auto-mode-alist 'all)
  (global-treesit-auto-mode t))

;; Markdown
(use-package markdown-mode
  :defer t
  :config
  (defvar nb/current-line '(0 . 0)
    "(start . end) of current line in current buffer")
  (make-variable-buffer-local 'nb/current-line)

  (defun nb/unhide-current-line (limit)
    "Font-lock function"
    (let ((start (max (point) (car nb/current-line)))
          (end (min limit (cdr nb/current-line))))
      (when (< start end)
        (remove-text-properties start end
                                '(invisible t display "" composition ""))
        (goto-char limit)
        t)))

  (defun nb/refontify-on-linemove ()
    "Post-command-hook"
    (let* ((start (line-beginning-position))
           (end (line-beginning-position 2))
           (needs-update (not (equal start (car nb/current-line)))))
      (setq nb/current-line (cons start end))
      (when needs-update
        (font-lock-fontify-block 3))))

  (defun nb/markdown-unhighlight ()
    "Enable markdown concealling"
    (interactive)
    (markdown-toggle-markup-hiding 'toggle)
    (font-lock-add-keywords nil '((nb/unhide-current-line)) t)
    (add-hook 'post-command-hook #'nb/refontify-on-linemove nil t))
  :custom-face
  (markdown-header-delimiter-face ((t (:height 0.9))))
  (markdown-header-face-1 ((t (:height 1.6  :weight extra-bold :inherit markdown-header-face))))
  (markdown-header-face-2 ((t (:height 1.4  :weight extra-bold :inherit markdown-header-face))))
  (markdown-header-face-3 ((t (:height 1.2  :weight extra-bold :inherit markdown-header-face))))
  (markdown-header-face-4 ((t (:height 1.15 :weight bold :inherit markdown-header-face))))
  (markdown-header-face-5 ((t (:height 1.1  :weight bold :inherit markdown-header-face))))
  (markdown-header-face-6 ((t (:height 1.05 :weight semi-bold :inherit markdown-header-face))))
  :hook
  (markdown-mode . abbrev-mode)
  (markdown-mode . variable-pitch-mode)
  (markdown-mode . nb/markdown-unhighlight))

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
  (typst-ts-mode-enable-raw-blocks-highlight t))
