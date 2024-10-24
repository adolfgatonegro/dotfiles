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

;; Prefer loading newer compiled files
(setq load-prefer-newer t)

;; Font compacting can be very resource-intensive, especially when rendering
;; icon fonts on Windows. This will increase memory usage.
(setq inhibit-compacting-font-caches t)

(unless (daemonp)
  (let ((old-value (default-toplevel-value 'file-name-handler-alist)))
    (set-default-toplevel-value
     'file-name-handler-alist
     ;; Determine the state of bundled libraries using calc-loaddefs.el.
     ;; If compressed, retain the gzip handler in `file-name-handler-alist`.
     ;; If compiled or neither, omit the gzip handler during startup for
     ;; improved startup and package load time.
     (if (eval-when-compile
           (locate-file-internal "calc-loaddefs.el" load-path))
         nil
       (list (rassq 'jka-compr-handler old-value))))
    ;; Ensure the new value persists through any current let-binding.
    (set-default-toplevel-value 'file-name-handler-alist
                                file-name-handler-alist)
    ;; Remember the old value to reset it as needed.
    (add-hook 'elpaca-after-init-startup-hook
              (lambda ()
                (set-default-toplevel-value
                 'file-name-handler-alist
                 ;; Merge instead of overwrite to preserve any changes made
                 ;; since startup.
                 (delete-dups (append file-name-handler-alist old-value))))
              101))
        (setq command-line-x-option-alist nil))

;; Native compilation and Byte compilation
(if (and (featurep 'native-compile)
         (fboundp 'native-comp-available-p)
         (native-comp-available-p))
    ;; Activate `native-compile'
    (setq native-comp-jit-compilation t
          native-comp-deferred-compilation t  ; Obsolete since Emacs 29.1
          package-native-compile t)
  ;; Deactivate the `native-compile' feature if it is not available
  (setq features (delq 'native-compile features)))

(setq byte-compile-warnings '(not obsolete))
(setq native-comp-async-report-warnings-errors 'silent)
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

;; Add frame borders and window dividers (from org-modern)
(modify-all-frames-parameters
 '((right-divider-width . 20)
   (internal-border-width . 20)))
(dolist (face '(window-divider
                window-divider-first-pixel
                window-divider-last-pixel))
  (face-spec-reset-face face)
  (set-face-foreground face (face-attribute 'default :background)))
(set-face-background 'fringe (face-attribute 'default :background))

(setq frame-resize-pixelwise t)
(setq default-frame-alist '((fullscreen . maximized)
                            (vertical-scroll-bars . nil)
                            (horizontal-scroll-bars . nil)
                            (background-color . "#1a1a26")
                            (ns-appearance . dark)
                            (alpha . 100)
                            (alpha-background . 100)
                            (ns-transparent-titlebar . t)))

(provide 'early-init)

;;; early-init.el ends here
