;;; dev-lsp.el --- LSP configuration -*- lexical-binding: t -*-

(use-package lsp-mode
  :ensure t
  :commands lsp
  :custom
  (lsp-idle-delay 0.2)
  (lsp-log-io nil)
  (lsp-completion-provider :none)
  (lsp-headerline-breadcrumb-enable t)
  (lsp-enable-symbol-highlighting t)
  (lsp-enable-on-type-formatting t)
  (lsp-enable-folding t)
  (lsp-enable-snippet t)
  (lsp-keep-workspace-alive nil)
  (lsp-eldoc-enable-hover t)
  (lsp-eldoc-render-all t)
  (lsp-signature-auto-activate t)             ; Show function signatures
  (lsp-signature-render-documentation t)      ; Include doc string in signature
  (setq lsp-modeline-code-actions-segments '(count icon name))
  (lsp-enable-file-watchers t)
  :bind (:map lsp-mode-map
        ("C-c l" . lsp-command-map)
        ("C-c l f" . lsp-format-buffer)
        ("C-c l r" . lsp-rename)
        ("C-c l a" . lsp-execute-code-action))
  :hook
  ((prog-mode . (lambda ()
                  (unless (derived-mode-p 'emacs-lisp-mode)
                    (lsp))))
   (lsp-mode . lsp-enable-which-key-integration)))

(use-package lsp-ui
  :ensure t
  :after lsp-mode
  :custom
  ;; Sideline configuration for errors
  (lsp-ui-sideline-enable t)
  (lsp-ui-sideline-show-diagnostics t)      ; Show errors in sideline
  (lsp-ui-sideline-delay 0.2)               ; Faster error display
  (lsp-ui-sideline-diagnostic-max-lines 5)  ; Show more error lines
  (lsp-ui-sideline-diagnostic-max-line-length 100) ; Longer error messages
  (lsp-ui-sideline-update-mode 'line)       ; Update diagnostics per line

  ;; Configure modeline diagnostics
  (lsp-modeline-diagnostics-enable t)       ; Show error stats in modeline
  (lsp-modeline-diagnostics-scope :file)    ; Show file-level diagnostics

  ;; Disable popups, show in minibuffer instead
  (lsp-ui-doc-enable nil)
  (lsp-ui-doc-show-with-cursor nil)

  ;; Enable eldoc for minibuffer documentation
  (lsp-eldoc-enable-hover t)           ; Show hover doc in minibuffer
  (lsp-eldoc-render-all t)             ; Show all documentation in minibuffer

  (lsp-idle-delay 0.2)
  (lsp-signature-auto-activate t)      ; Show function signatures
  (lsp-signature-render-documentation t) ; Include doc string in signature

  (lsp-ui-peek-enable t)
  (lsp-ui-sideline-enable t)
  (lsp-ui-sideline-show-hover t)
  (lsp-ui-sideline-show-code-actions nil)   ; Disable code actions in sideline
  (lsp-ui-imenu-auto-refresh t)
  :config
  (eldoc-mode 1)
  (lsp-ui-sideline-mode 1))

(use-package eldoc
  :custom
  (eldoc-echo-area-use-multiline-p t)
  (eldoc-echo-area-prefer-doc-buffer t)
  (eldoc-idle-delay 0.2)
  (eldoc-echo-area-display-truncation-message nil)
  (eldoc-documentation-strategy #'eldoc-documentation-compose-eagerly)
  :config
  (global-eldoc-mode 1))

;; LSP-Treemacs integration
(use-package lsp-treemacs
  :ensure t
  :after (lsp-mode treemacs)
  :commands lsp-treemacs-errors-list
  :custom
  (lsp-treemacs-sync-mode 1)
  :bind
  (:map lsp-mode-map
        ("C-c t e" . lsp-treemacs-errors-list)
        ("C-c t s" . lsp-treemacs-symbols)
        ("C-c t r" . lsp-treemacs-references)
        ("C-c t i" . lsp-treemacs-implementations)
        ("C-c t c" . lsp-treemacs-call-hierarchy))
  :config
  ;; Enable bidirectional sync
  (lsp-treemacs-sync-mode 1))

(use-package flymake
  :hook (prog-mode . flymake-mode)
  :custom
  (flymake-no-changes-timeout 0.5)
  (flymake-start-on-save-buffer t)
  :bind
  (:map flymake-mode-map
        ("C-c ! n" . flymake-goto-next-error)
        ("C-c ! p" . flymake-goto-prev-error)
        ("C-c ! l" . flymake-show-buffer-diagnostics)))

(add-hook 'lsp-completion-mode-hook
          (lambda ()
            (setf (alist-get 'styles (alist-get 'lsp-capf completion-category-defaults))
                  '(orderless))))
                  
(provide 'dev-lsp)
