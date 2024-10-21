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
