;;; lang-python.el --- Python configuration -*- lexical-binding: t -*-

(require 'core-treesitter)

;; Add Python grammar source
(add-to-list 'treesit-language-source-alist
             '(python "https://github.com/tree-sitter/tree-sitter-python"))

(use-package python-ts-mode
  :straight nil  ;; Built-in mode, no need to install
  :mode "\\.py\\'"
  :hook (python-ts-mode . lsp-deferred)
  :config
  (setq python-ts-mode-indent-offset 4)
  (setq treesit-font-lock-level 4))

(provide 'lang-python)