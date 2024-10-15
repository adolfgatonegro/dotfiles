;; we're using elpaca, so disable package.el here
(setq package-enable-at-startup nil)

;; performance Hacks
(setq byte-compile-warnings '(not obsolete))
(setq gc-cons-threshold 10000000)
(setq native-comp-async-report-warnings-errors 'silent)
(setq read-process-output-max (* 1024 1024 4))
(setq warning-suppress-log-types '((comp) (bytecomp)))

;; minimal ui
(menu-bar-mode -1) ;; disable menu bar
(tool-bar-mode -1) ;; disable toolbar
(tooltip-mode -1) ;; disable tooltips
(scroll-bar-mode -1) ;; disable scrollbar
(pixel-scroll-precision-mode 1) ;; enable smooth scrolling

(setq inhibit-startup-echo-area-message (user-login-name)) ;; silence startup message

(setq inhibit-splash-screen t ;; disable splash screen
use-file-dialog nil ;; don't use system file dialog
tab-bar-new-button-show nil ;; don't show new tab button
tab-bar-close-button-show nil ;; don't show tab close button
tab-line-close-button-show nil) ;; don't show tab close button

;; Default frame configuration: full screen, good-looking title bar on macOS
(setq frame-resize-pixelwise t)
(setq default-frame-alist '((fullscreen . maximized)
                            ;; Setting the face in here prevents flashes of
                            ;; color as the theme gets activated
                            (background-color . "#0a0a15")
                            (ns-appearance . dark)
                            (ns-transparent-titlebar . t)))

;; frame transparency parameters
(set-frame-parameter (selected-frame) 'alpha '(100 . 100))
(add-to-list 'default-frame-alist '(alpha . (100 . 100)))
(set-frame-parameter nil 'alpha-background 90)
(add-to-list 'default-frame-alist '(alpha-background . 90))
