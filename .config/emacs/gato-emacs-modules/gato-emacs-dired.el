;; * dired
(use-package dired
  :ensure nil
  :custom
  (dired-listing-switches "-lah --group-directories-first") ;; Display files in a human-readable format and group directories first.
  (dired-dwim-target t) ;; Enable "do what I mean" for target directories.
  (dired-guess-shell-alist-user
   '((".*" "open" "xdg-open"))) ;; Use xdg-open to open everything.
  (dired-kill-when-opening-new-dired-buffer t)) ;; Close the previous buffer when opening a new `dired' instance.

;; * dired-+
(use-package dired-x
  :ensure nil
  :after dired)

;; * dired-open
(use-package dired-open
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
                     "\\)"))
)

;; * provide module
(provide 'gato-emacs-dired)
