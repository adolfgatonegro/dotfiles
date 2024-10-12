;; * org mode

;; ** org
(use-package org
  :ensure nil
  :defer t
  :init
  ;; edit settings (recommended by org-modern)
  (setq org-auto-align-tags nil
	      org-tags-column 0
	      org-catch-invisible-edits 'show-and-error
	      org-special-ctrl-a/e t ;; special navigation behaviour in headlines
	      org-insert-heading-respect-content t)

  ;; styling, hide markup, etc. (recommended by org-modern)
  (setq org-hide-emphasis-markers t
	      org-src-fontify-natively t ;; fontify source blocks natively
	      org-highlight-latex-and-related '(native) ;; fontify latex blocks natively
	      org-pretty-entities t)

  ;; agenda styling (recommended by org-modern)
  (setq org-agenda-tags-column 0
	      org-agenda-block-separator ?─
	      org-agenda-time-grid
	      '((daily today require-timed)
	        (800 1000 1200 1400 1600 1800 2000)
	        " ┄┄┄┄┄ " "┄┄┄┄┄┄┄┄┄┄┄┄┄┄┄")
	      org-agenda-current-time-string
	      "⭠ now ─────────────────────────────────────────────────")

  (setq org-ellipsis "..."))

;; ** org-tempo
(require 'org-tempo)

;; ** toc-org
(use-package toc-org
  :after org
  :commands toc-org-enable
  :init (add-hook 'org-mode-hook 'toc-org-enable))

;; ** org-bullets
(add-hook 'org-mode-hook 'org-indent-mode)
(use-package org-bullets
  :after org)
(add-hook 'org-mode-hook (lambda () (org-bullets-mode 1)))

;; * provide module
(provide 'gato-emacs-org-mode)
