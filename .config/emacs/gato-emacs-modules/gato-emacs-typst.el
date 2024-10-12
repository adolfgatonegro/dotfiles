;; * typst support

;; add full support for typst, including syntax highlighting and lsp functionality

;; requires:
;; - `typst-ts-mode': typst treesitter major mode
;;   https://codeberg.org/meow_king/typst-ts-mode/
;; - `tinymist': language service for typst (binary should be in `'$PATH')
;;   https://github.com/Myriad-Dreamin/tinymist
;; more info:
;; - `lsp-mode': adding a new language
;;    https://emacs-lsp.github.io/lsp-mode/page/adding-new-language/

(use-package typst-ts-mode
  :defer t
  :ensure (:type git :host codeberg :repo "meow_king/typst-ts-mode"
                 :files (:defaults "*.el"))
  :custom
  (typst-ts-watch-options "--open")
  (typst-ts-mode-grammar-location (expand-file-name "tree-sitter/libtree-sitter-typst.so" user-emacs-directory))
  (typst-ts-mode-enable-raw-blocks-highlight t)

  ;; Register `tinymist' as the Typst language server
  :config
  (with-eval-after-load 'lsp-mode
    (add-to-list 'lsp-language-id-configuration
                 '(typst-ts-mode . "typst"))
  
    (lsp-register-client
     (make-lsp-client :new-connection (lsp-stdio-connection "tinymist")
                      :activation-fn (lsp-activate-on "typst")
                      :server-id 'tinymist)))
  )

;; * provide module
(provide 'gato-emacs-typst)
