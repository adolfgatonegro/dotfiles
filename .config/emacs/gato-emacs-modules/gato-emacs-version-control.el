;; * version control

;; ** diff-hl
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

;; * provide module
(provide 'gato-emacs-version-control)
