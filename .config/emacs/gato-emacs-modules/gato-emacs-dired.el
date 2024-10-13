;; * dired

(use-package dired
  :ensure nil                                                ;; This is built-in, no need to fetch it.
  :custom
  (dired-listing-switches "-lah --group-directories-first")  ;; Display files in a human-readable format and group directories first.
  (dired-dwim-target t)                                      ;; Enable "do what I mean" for target directories.
  (dired-guess-shell-alist-user
   '(("\\.\\(png\\|jpe?g\\|tiff\\)" "nsxiv" "xdg-open" "open") ;; Open image files with `nsxiv' or the default viewer.
     ("\\.\\(mp[34]\\|m4a\\|ogg\\|flac\\|webm\\|mkv\\)" "mpv" "xdg-open" "open") ;; Open audio and video files with `mpv'.
     (".*" "open" "xdg-open")))                              ;; Default opening command for other files.
  (dired-kill-when-opening-new-dired-buffer t))               ;; Close the previous buffer when opening a new `dired' instance.

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
