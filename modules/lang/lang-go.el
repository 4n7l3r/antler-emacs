;;; lang-go.el --- Go development setup -*- lexical-binding: t -*-
(require 'dev-tools)

;; Use tree-sitter mode for Go in Emacs 29+
(when (>= emacs-major-version 29)
  ;; Add Go grammar source for tree-sitter
  (add-to-list 'treesit-language-source-alist
               '(go "https://github.com/tree-sitter/tree-sitter-go"))
  
  ;; Install the grammar if needed
  (unless (treesit-language-available-p 'go)
    (treesit-install-language-grammar 'go)))

;; Configure go-ts-mode (tree-sitter based) for Emacs 29+
(use-package go-ts-mode
  :when (>= emacs-major-version 29)
  :mode "\\.go\\'"
  :hook (go-ts-mode . lsp-deferred)
  :custom
  (go-ts-mode-indent-offset 4)
  :config
  ;; Use tree-sitter for font-lock
  (setq treesit-font-lock-level 4))

;; Traditional go-mode for older Emacs versions
(use-package go-mode
  :straight t
  :when (< emacs-major-version 29)
  :mode "\\.go\\'"
  :hook ((before-save . gofmt-before-save)
         (go-mode . lsp-deferred))
  :custom
  (gofmt-command "gofmt")
  (godoc-at-point-function #'godoc-gogetdoc)
  :config
  ;; Configure indentation
  (add-hook 'go-mode-hook
            (lambda ()
              (setq-local tab-width 4)
              (setq-local indent-tabs-mode t))))

;; Go utilities
(use-package go-eldoc
  :straight t
  :hook (go-mode . go-eldoc-setup))

;; Go tag management
(use-package go-tag
  :straight t
  :after (go-mode go-ts-mode)
  :bind (:map go-mode-map
              ("C-c g t" . go-tag-add)
              ("C-c g T" . go-tag-remove)
              :map go-ts-mode-map
              ("C-c g t" . go-tag-add)
              ("C-c g T" . go-tag-remove))
  :custom
  (go-tag-args '("-transform" "camelcase")))

;; Go testing
(use-package gotest
  :straight t
  :after (go-mode go-ts-mode) 
  :bind (("C-c g p" . go-test-current-project)
         ("C-c g f" . go-test-current-file)
         ("C-c g t" . go-test-current-test)
         ("C-c g c" . go-test-current-coverage)
         ("C-c g b" . go-test-current-benchmark)
         ("C-c g r" . go-run)))

;; Go playground
(use-package go-playground
  :straight t
  :commands (go-playground)
  :custom
  (go-playground-basedir "~/go/playground"))

;; Go doc integration
(use-package go-impl
  :straight t
  :commands (go-impl))

;; Configure lsp-mode for Go
(with-eval-after-load 'lsp-mode
  (add-to-list 'lsp-file-watch-ignored-directories "[/\\\\]vendor$")
  (add-to-list 'lsp-file-watch-ignored-directories "[/\\\\]node_modules$")
  
  (lsp-register-custom-settings
   '(("gopls.completeUnimported" t t)
     ("gopls.staticcheck" t t)
     ("gopls.allExperiments" t t)
     ("gopls.matcher" "fuzzy" t)
     ("gopls.usePlaceholders" t t))))

(provide 'lang-go)
