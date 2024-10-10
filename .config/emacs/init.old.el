;; init.el
;; gatoneg.ro
;; Config based on System Crafters

;; UI configuration
(setq inhibit-startup-message t)        ; Disable splash screen
(menu-bar-mode 0)			; Disable menu bar
(tool-bar-mode 0)	       		; Disable tool bar
(scroll-bar-mode 0)			; Disable scrollbar
(tooltip-mode 0)			; Disable tooltips
(set-fringe-mode 10)

;; Set frame transparency
(set-frame-parameter (selected-frame) 'alpha '(100 . 95))
(add-to-list 'default-frame-alist '(alpha . (100 . 95)))
(set-frame-parameter nil 'alpha-background 90)
(add-to-list 'default-frame-alist '(alpha-background . 90))

;; Set font
(set-face-attribute 'default nil :font "monospace" :height 100)

;(load-theme 'deeper-blue)

(column-number-mode)		           ; Enable column numbers
(global-display-line-numbers-mode 1)       ; Display line numbers
(setq display-line-numbers-type 'relative) ; Show relative numbers

(dolist (mode '(org-mode-hook
		term-mode-hook
		shell-mode-hook
		eshell-mode-hook))
  (add-hook mode (lambda () (display-line-numbers-mode 0))))

;; Do not use dialog boxes for prompting
(setq use-dialog-box nil)

;; Move customisation variables to a separate file
(setq custom-file (locate-user-emacs-file "custom-vars.el"))
(load custom-file 'noerror 'nomessage)

;; Revert file and non-file buffers on disk changes
(global-auto-revert-mode 1)
(setq global-auto-revert-non-file-buffers t)

;; Save minibuffer prompts history
(setq history-length 25)
(savehist-mode 1)

;; Other stuff
(recentf-mode 1)       ; Track recently opened files
(indent-tabs-mode 0)   ; Do not insert TABs in indents
(save-place-mode 1)    ; Remember cursor position in file
(electric-pair-mode 1) ; Enable automatic parens pairing

;; KEY BINDINGS
(global-set-key (kbd "<escape>") 'keyboard-escape-quit)
(global-set-key (kbd "C-M-j") 'counsel-switch-buffer)

;; PACKAGES
;; Initialise package sources
(require 'package)
(setq package-archives '(("melpa" . "https://melpa.org/packages/")
                         ("org" . "https://orgmode.org/elpa/")
                         ("elpa" . "https://elpa.gnu.org/packages/")))
(package-initialize)

;; Refresh package sources
(unless package-archive-contents
  (package-refresh-contents))

;; Init use-package
(require 'use-package)

;; Always ensure that packages are installed
(setq use-package-always-ensure t)

;; PACKAGE: counsel
;; Ivy-enhanced versions of common Emacs commands
(use-package counsel
  :defer 1
  :diminish
  :bind (("C-s" . swiper)
	 ("C-x C-f" . counsel-find-file)
         ("C-x b" . counsel-ibuffer)
         ("C-r" . counsel-minibuffer-history)
         :map ivy-minibuffer-map
         ("TAB" . ivy-alt-done)	
         ("C-l" . ivy-alt-done)
         ("C-j" . ivy-next-line)
         ("C-k" . ivy-previous-line)
         :map ivy-switch-buffer-map
         ("C-k" . ivy-previous-line)
         ("C-l" . ivy-done)
         ("C-d" . ivy-switch-buffer-kill)
         :map ivy-reverse-i-search-map
         ("C-k" . ivy-previous-line)
         ("C-d" . ivy-reverse-i-search-kill))
  :config
  :hook(
	(after-init . ivy-mode)
        (after-init . counsel-mode)))

;; PACKAGE: ivy-rich
;; A friendlier interface for Ivy
(use-package ivy-rich
  :defer t
  :hook
  (ivy-mode . ivy-rich-mode))

;; PACKAGE: doom-themes
;; Theme megapack for Emacs
(use-package doom-themes
  :config
  ;; Global settings (defaults)
  (setq doom-themes-enable-bold t    ; if nil, bold is universally disabled
        doom-themes-enable-italic t) ; if nil, italics is universally disabled
  (load-theme 'doom-tokyo-night t)
  ;; Enable flashing mode-line on errors
  (doom-themes-visual-bell-config)
  ;; Corrects (and improves) org-mode's native fontification.
  (doom-themes-org-config))

;; PACKAGE: doom-modeline
;; Fancy, fast, minimalist modeline for Emacs
(use-package doom-modeline
  :defer t
  :init (setq doom-modeline-height 25
              doom-modeline-bar-width 2)
  :hook (after-init . doom-modeline-mode))

;; PACKAGE: all-the-icons
;; Utility package to collect various Icon Fonts and propertize them within Emacs.
;; Important: run `M-x all-the-icons-install-fonts` after the first time the config
;; is loaded on a new system to download the resource fonts.
(use-package all-the-icons
  :if (display-graphic-p))

;; PACKAGE: rainbow-delimiters
;; A rainbow parentheses-like mode for delimiters
(use-package rainbow-delimiters
  :defer t
  :hook (prog-mode . rainbow-delimiters-mode))

;; PACKAGE: which-key
;; Minor mode that displays available keybindings in a popup
(use-package which-key
  :defer t
  :config
  (setq which-key-idle-delay 0.25)
  :hook
  (after-init . which-key-mode))

;; PACKAGE: helpful
;; A better Emacs *help* buffer 
(use-package helpful
  :defer t
  :hook
  (help-mode)
  :custom
  (counsel-describe-function-function #'helpful-callable)
  (counsel-describe-variable-function #'helpful-variable)
  :bind
  ([remap describe-function] . counsel-describe-function)
  ([remap describe-command] . helpful-command)
  ([remap describe-variable] . counsel-describe-variable)
  ([remap describe-key] . helpful-key))

;; PACKAGE: evil
;; The extensible vi layer for Emacs.
(use-package evil
  :defer t
  :hook
  (after-init . evil-mode)
  :init
  (setq evil-want-integration t)
  (setq evil-want-keybinding nil)
  (setq evil-want-C-u-scroll t)
  (setq evil-want-C-d-scroll t)
  :config
  (define-key evil-insert-state-map (kbd "C-g") 'evil-normal-state)
  (define-key evil-insert-state-map (kbd "C-h") 'evil-delete-backward-char-and-join)
  (define-key evil-visual-state-map (kbd "g c") 'comment-or-uncomment-region)

  ;; Use visual line motions even outside of visual-line-mode-buffers
  (evil-global-set-key 'motion "j" 'evil-next-visual-line)
  (evil-global-set-key 'motion "k" 'evil-previous-visual-line)

  (evil-set-initial-state 'messages-buffer-mode 'normal)
  (evil-set-initial-state 'dashboard-mode 'normal))

(use-package evil-collection
  :defer t
  :after evil
  :config
  :hook
  (evil-mode . evil-collection-init))
