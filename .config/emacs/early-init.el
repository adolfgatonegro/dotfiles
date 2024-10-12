;; we're using elpaca, so disable package.el here
(setq package-enable-at-startup nil)

;; frame transparency parameters
(set-frame-parameter (selected-frame) 'alpha '(100 . 100))
(add-to-list 'default-frame-alist '(alpha . (100 . 100)))
(set-frame-parameter nil 'alpha-background 90)
(add-to-list 'default-frame-alist '(alpha-background . 90))

;; performance Hacks
(setq gc-cons-threshold #x40000000)
(setq read-process-output-max (* 1024 1024 4))

;; minimal ui
(menu-bar-mode -1) ;; disable menu bar
(tool-bar-mode -1) ;; disable toolbar
(tooltip-mode -1) ;; disable tooltips
(scroll-bar-mode -1) ;; disable scrollbar
(pixel-scroll-precision-mode 1) ;; enable smooth scrolling

(setq inhibit-splash-screen t ;; disable splash screen
use-file-dialog nil ;; don't use system file dialog
tab-bar-new-button-show nil ;; don't show new tab button
tab-bar-close-button-show nil ;; don't show tab close button
tab-line-close-button-show nil) ;; don't show tab close button
