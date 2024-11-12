;;; lang-go.el --- Go development setup -*- lexical-binding: t -*-
(require 'dev-tools)

(use-package go-mode
  :ensure t
  :hook ((before-save . gofmt-before-save))
  :custom
  (gofmt-command "gofmt")
  (godoc-at-point-function #'godoc-gogetdoc)
  :config
  ;; Key bindings
  :bind (:map go-mode-map
              ("C-c C-t" . go-test-current-file)
              ("C-c C-f" . go-test-current-test)
              ("C-c C-p" . go-test-current-project)
              ("C-c C-b" . go-run)))
;; (add-hook 'go-mode-hook
;;             (lambda ()
;;               (setq-local tab-width 4)
;;               (setq-local indent-tabs-mode t)
;;               ;; Ensure electric indent works properly
;;               (setq-local electric-indent-chars '(?\n ?\} ?\{))))

;; Add Go grammar source
(add-to-list 'treesit-language-source-alist
             '(go "https://github.com/tree-sitter/tree-sitter-go"))

(use-package go-ts-mode
  :after lsp
  :hook (go-ts-mode . lsp-deferred)
  :config
  ;; Configure indentation
  (setq go-ts-mode-indent-offset 4)
  ;; Use tree-sitter for font-lock
  (setq treesit-font-lock-level 4))

(provide 'lang-go)
