;;; core-keybindings.el --- Global keybindings -*- lexical-binding: t -*-

;; Window management
(global-set-key (kbd "C-x |") 'split-window-right)
(global-set-key (kbd "C-x _") 'split-window-below)
(global-set-key (kbd "C-x }") 'enlarge-window-horizontally)
(global-set-key (kbd "C-x {") 'shrink-window-horizontally)
(global-set-key (kbd "C-x ^") 'enlarge-window)

;; Buffer management
(global-set-key (kbd "C-x C-b") 'ibuffer)
(global-set-key (kbd "C-x k") 'kill-this-buffer)

;; Navigation
(global-set-key (kbd "M-g g") 'avy-goto-line)
(global-set-key (kbd "C-;") 'avy-goto-char-timer)

;; Text manipulation
(global-set-key (kbd "M-/") 'hippie-expand)
(global-set-key (kbd "C-c d") 'duplicate-line)

;; LSP keybindings
(with-eval-after-load 'lsp-mode
  (define-key lsp-mode-map (kbd "C-c l") lsp-command-map)
  (define-key lsp-mode-map (kbd "C-c l f") 'lsp-format-buffer)
  (define-key lsp-mode-map (kbd "C-c l r") 'lsp-rename)
  (define-key lsp-mode-map (kbd "C-c l a") 'lsp-execute-code-action))

(provide 'core-keybindings)
