;; Disable `package.el' here, required to use Elpaca later
(setq package-enable-at-startup nil)

;; Temporarily increase garbage collection threshold to reduce
;; startup time. We will change this value to something less
;; aggressive once we're done with startup.
(setq gc-cons-threshold most-positive-fixnum
      gc-cons-percentage 0.5)

;; Lifted from Prot's `early-init.el', we store the default
;; values, set them to `nil' for startup, then restore them.
(defvar gato--file-name-handler-alist file-name-handler-alist)
(defvar gato--vc-handled-backends vc-handled-backends)

(setq file-name-handler-alist nil
      vc-handled-backends nil)

(add-hook 'elpaca-after-init-hook
          (lambda ()
            (setq gc-cons-threshold (* 1000 1000 8)
                  gc-cons-percentage 0.1
                  file-name-handler-alist gato--file-name-handler-alist
                  vc-handled-backends gato--vc-handled-backends)))

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

(setq frame-resize-pixelwise t
      frame-inhibit-implied-resize t
      frame-title-format '("%b")
      use-dialog-box nil
      use-file-dialog nil
      use-short-answers t
      inhibit-splash-screen t
      inhibit-startup-screen t
      inhibit-startup-echo-area-message user-login-name
      inhibit-startup-buffer-menu t)

;;; Minimal UI
(menu-bar-mode -1)
(tool-bar-mode -1)
(tooltip-mode -1)
(scroll-bar-mode -1)
(pixel-scroll-precision-mode 1)

;;; Set frame alpha
(push '(alpha . 100) default-frame-alist)
(push '(alpha-background . 100) default-frame-alist)
