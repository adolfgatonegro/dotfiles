;; * dashboard

(use-package dashboard
  :defer t
  :init
  (setq initial-buffer-choice 'dashboard-open
        dashboard-set-heading-icons t
        dashboard-set-file-icons t
        dashboard-banner-logo-title "«Objects such as corpses, painful to view in themselves, can become delightful to contemplate.»"
        ;;dashboard-startup-banner 'logo ;; use standard emacs logo as banner
        dashboard-startup-banner (concat user-emacs-directory "themes/gatonegro.png")
        dashboard-projects-backend 'projectile
        dashboard-center-content t ;; set to 't' for centered content
        dashboard-items '((recents . 5)
                          (agenda . 5 )
                          (bookmarks . 3)
                          (projects . 3)
                          (registers . 3)))
  :custom
  (dashboard-modify-heading-icons '((recents . "file-text")
                                    (bookmarks . "book")))
  :config
  (dashboard-setup-startup-hook))

;; * provide module
(provide 'gato-emacs-dashboard)
