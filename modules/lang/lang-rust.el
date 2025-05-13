;;; lang-rust.el --- Rust configuration -*- lexical-binding: t -*-

(use-package rust-mode
  :straight t
  :hook ((rust-mode . lsp-deferred)
	 (rust-mode . prettify-symbols-mode)
         (rust-mode . (lambda ()
			(setq indent-tabs-mode nil)
			(setq tab-width 4)
			(setq rust-format-on-save t))))
  :init
  (setq rust-mode-treesitter-derive t)
  :custom
  ;; Format settings
  (rust-format-on-save t)
  (rust-format-show-buffer nil)
  (rust-indent-offset 4)

  :config
  ;; Custom format function
  (defun my/rust-format-buffer ()
    "Format buffer and save."
    (interactive)
    (rust-format-buffer)
    (save-buffer))

  :bind (:map rust-mode-map
	      ("C-c C-c" . rust-run)
	      ("C-c C-t" . rust-test)
	      ("C-c C-f" . my/rust-format-buffer)
	      ("C-c C-k" . rust-check)
	      ("C-c C-b" . rust-compile)))

;; LSP configuration for Rust
(use-package lsp-mode
  :hook (rust-mode . lsp-deferred)
  :custom
  ;; Rust analyzer settings
  (lsp-rust-analyzer-server-display-inlay-hints t)
  (lsp-rust-analyzer-display-lifetime-elision-hints-use-parameter-names nil)
  (lsp-rust-analyzer-display-chaining-hints t)
  (lsp-rust-analyzer-display-lifetime-elision-hints-enable "skip_trivial")
  (lsp-rust-analyzer-display-closure-return-type-hints t)
  (lsp-rust-analyzer-display-parameter-hints nil)
  (lsp-rust-analyzer-display-reborrow-hints nil)
  (lsp-rust-analyzer-cargo-watch-command "clippy"))

(provide 'lang-rust)
