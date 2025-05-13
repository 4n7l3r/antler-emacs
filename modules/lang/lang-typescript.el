;;; lang-typescript.el --- TypeScript configuration -*- lexical-binding: t -*-

(require 'core-treesitter)

;; Add TypeScript grammars
(add-to-list 'treesit-language-source-alist
             '(typescript "https://github.com/tree-sitter/tree-sitter-typescript" "master" "typescript/src"))
(add-to-list 'treesit-language-source-alist
             '(tsx "https://github.com/tree-sitter/tree-sitter-typescript" "master" "tsx/src"))

(use-package typescript-ts-mode
  :straight nil  ;; Built-in mode, no need to install
  :mode (("\\.ts\\'" . typescript-ts-mode)
         ("\\.tsx\\'" . tsx-ts-mode))
  :hook ((typescript-ts-mode . lsp-deferred)
         (tsx-ts-mode . lsp-deferred))
  :config
  (setq typescript-ts-mode-indent-offset 2)
  (setq treesit-font-lock-level 4))

(provide 'lang-typescript)