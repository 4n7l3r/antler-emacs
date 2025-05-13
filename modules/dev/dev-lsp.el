;;; dev-lsp.el --- LSP configuration -*- lexical-binding: t -*-

;; LSP performance setup
(defun antler/lsp-mode-setup-completion ()
  "Setup completion for LSP mode."
  (setf (alist-get 'styles (alist-get 'lsp-capf completion-category-defaults))
        '(orderless)))

;; Detect and set up multi-root project workspace
(defun antler/lsp-setup-multi-workspace ()
  "Setup workspace folders for multi-root projects."
  (when-let* ((project-root (project-root (project-current))))
    (let ((workspaces '())
          (git-folders '()))
      ;; Find Git repositories in project
      (when (executable-find "find")
        (with-temp-buffer
          (let ((default-directory project-root))
            (when (eq 0 (call-process "find" nil t nil "." "-type" "d" "-name" ".git" "-not" "-path" "*/node_modules/*" "-not" "-path" "*/vendor/*"))
              (goto-char (point-min))
              (while (re-search-forward "^./\\(.*\\)/.git$" nil t)
                (push (expand-file-name (match-string 1) project-root) git-folders)))))
        ;; Add workspace folders
        (when (> (length git-folders) 1)
          (dolist (folder git-folders)
            (push (lsp-workspace-folders-add folder) workspaces))
          workspaces)))))

(use-package lsp-mode
  :straight t
  :commands lsp
  :hook
  ((prog-mode . (lambda ()
                  (unless (derived-mode-p 'emacs-lisp-mode)
                    (lsp-deferred))))
   (lsp-completion-mode . antler/lsp-mode-setup-completion)
   (lsp-mode . lsp-enable-which-key-integration)
   (lsp-mode . antler/lsp-setup-multi-workspace))
  :custom
  ;; Performance settings
  (lsp-idle-delay 0.2)
  (lsp-log-io nil)
  (lsp-completion-provider :none) ; Use Corfu instead
  (lsp-keymap-prefix "C-c l")
  (lsp-keep-workspace-alive nil)
  (lsp-enable-file-watchers t)
  (lsp-file-watch-threshold 5000)
  
  ;; UI settings
  (lsp-headerline-breadcrumb-enable t)
  (lsp-headerline-breadcrumb-segments '(symbols file path))
  (lsp-lens-enable t)
  (lsp-enable-symbol-highlighting t)
  (lsp-enable-on-type-formatting t)
  (lsp-enable-folding t)
  (lsp-enable-indentation t)
  (lsp-enable-snippet t)
  (lsp-modeline-code-actions-segments '(count icon name))
  
  ;; Docs and signatures
  (lsp-eldoc-enable-hover t)
  (lsp-eldoc-render-all t)
  (lsp-signature-auto-activate t)
  (lsp-signature-render-documentation t)
  
  ;; Session management
  (lsp-enable-session-management t)
  (lsp-session-file (expand-file-name ".lsp-session" user-emacs-directory))
  
  ;; For Emacs 31, use eglot for some modes if desired
  (lsp-use-plists t)
  
  ;; Workspace folders
  (lsp-auto-configure t)
  (lsp-auto-guess-root t)
  (lsp-before-save-edits t)
  (lsp-enable-dap-auto-configure t)
  
  ;; Optimize for large files
  (lsp-enable-semantic-highlighting t)
  (lsp-semantic-tokens-enable t)
  (lsp-semantic-tokens-honor-refresh-requests t)
  :config
  
  ;; Server-specific configurations
  
  ;; Python - pyright
  (with-eval-after-load 'lsp-pyright
    (setq lsp-pyright-typechecking-mode "basic"
          lsp-pyright-auto-import-completions t
          lsp-pyright-use-library-code-for-types t))
  
  ;; TypeScript/JavaScript
  (with-eval-after-load 'lsp-javascript
    (setq lsp-javascript-format-enable t
          lsp-javascript-format-insert-space-after-opening-and-before-closing-nonempty-brackets nil
          lsp-typescript-format-enable t
          lsp-typescript-format-insert-space-after-opening-and-before-closing-nonempty-brackets nil))
  
  ;; Rust
  (with-eval-after-load 'lsp-rust
    (setq lsp-rust-analyzer-cargo-watch-command "clippy"
          lsp-rust-analyzer-proc-macro-enable t
          lsp-rust-analyzer-server-display-inlay-hints t
          lsp-rust-analyzer-display-lifetime-elision-hints-enable "always"
          lsp-rust-analyzer-display-chaining-hints t
          lsp-rust-analyzer-display-closure-return-type-hints t
          lsp-rust-analyzer-display-parameter-hints t
          lsp-rust-analyzer-display-reborrow-hints "always"))
  
  ;; Optimize for slow work machines
  (defun antler/lsp-optimize-for-slow-machines ()
    "Optimize LSP for slow performance."
    (interactive)
    (setq lsp-enable-file-watchers nil
          lsp-enable-folding nil
          lsp-enable-on-type-formatting nil
          lsp-lens-enable nil
          lsp-enable-symbol-highlighting nil
          lsp-signature-auto-activate nil
          lsp-enable-text-document-color nil
          lsp-enable-semantic-highlighting nil
          lsp-enable-indentation nil
          lsp-headerline-breadcrumb-enable nil))
  
  ;; Add to lsp command map
  (define-key lsp-command-map (kbd "O") 'antler/lsp-optimize-for-slow-machines))

;; Prettier LSP UI elements
(use-package lsp-ui
  :straight t
  :after lsp-mode
  :commands lsp-ui-mode
  :custom
  ;; Sideline configuration
  (lsp-ui-sideline-enable t)
  (lsp-ui-sideline-show-diagnostics t)
  (lsp-ui-sideline-delay 0.2)
  (lsp-ui-sideline-show-hover nil) ; Use dedicated box for hover
  (lsp-ui-sideline-update-mode 'line)
  (lsp-ui-sideline-ignore-duplicate t)

  ;; Doc/hover configuration - use a nice child frame
  (lsp-ui-doc-enable t)
  (lsp-ui-doc-position 'at-point)
  (lsp-ui-doc-delay 0.2)
  (lsp-ui-doc-show-with-cursor t)
  (lsp-ui-doc-show-with-mouse nil) ; Only show with cursor
  (lsp-ui-doc-use-childframe t)
  (lsp-ui-doc-include-signature t)
  (lsp-ui-doc-max-width 80)
  (lsp-ui-doc-max-height 20)
  
  ;; Peek functionality
  (lsp-ui-peek-enable t)
  (lsp-ui-peek-list-width 60)
  (lsp-ui-peek-peek-height 20)
  
  ;; Imenu
  (lsp-ui-imenu-enable t)
  (lsp-ui-imenu-auto-refresh t)
  :config
  (define-key lsp-ui-mode-map [remap xref-find-definitions] #'lsp-ui-peek-find-definitions)
  (define-key lsp-ui-mode-map [remap xref-find-references] #'lsp-ui-peek-find-references))

;; For Emacs 29+, use built-in eglot for some modes as an alternative
(use-package eglot
  :ensure nil ; built-in for Emacs 29+
  :when (>= emacs-major-version 29)
  :custom
  (eglot-sync-connect nil)
  (eglot-connect-timeout 10)
  (eglot-autoshutdown t)
  (eglot-extend-to-xref t)
  (eglot-events-buffer-size 0) ; Don't keep events buffer
  ;; Don't auto-enable, we use lsp-mode by default
  :config
  ;; Add safety checks to prevent eglot warnings
  (advice-add 'eglot-server-programs :around
              (lambda (orig-fun &rest args)
                "Safely get server programs with fallback."
                (or (apply orig-fun args) '())))
  :hook
  ((emacs-lisp-mode . (lambda ()
                        (unless (bound-and-true-p lsp-mode)
                          (condition-case err
                              (eglot-ensure)
                            (error
                             (message "Eglot initialization error: %s" 
                                     (error-message-string err)))))))))

;; Enhanced LSP integration with treemacs
(use-package lsp-treemacs
  :straight t
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
        ("C-c t c" . lsp-treemacs-call-hierarchy)))

;; Use consult with LSP for better search experience
(use-package consult-lsp
  :straight t
  :after (consult lsp-mode)
  :bind
  (:map lsp-mode-map
        ("C-c l s" . consult-lsp-symbols)
        ("C-c l f" . consult-lsp-file-symbols)
        ("C-c l d" . consult-lsp-diagnostics)))

;; Enhanced error display
(use-package flymake
  :hook (prog-mode . flymake-mode)
  :custom
  (flymake-no-changes-timeout 0.5)
  (flymake-start-on-save-buffer t)
  (flymake-proc-compilation-prevents-syntax-check nil)
  (flymake-wrap-around nil)
  :bind
  (:map flymake-mode-map
        ("C-c ! n" . flymake-goto-next-error)
        ("C-c ! p" . flymake-goto-prev-error)
        ("C-c ! l" . flymake-show-buffer-diagnostics)))

;; Enhance LSP diagnostics UI
(use-package flycheck
  :straight t
  :defer t
  :diminish
  :custom
  (flycheck-indication-mode 'right-fringe)
  (flycheck-check-syntax-automatically '(save idle-change mode-enabled))
  (flycheck-idle-change-delay 0.5)
  (flycheck-display-errors-delay 0.2))

(provide 'dev-lsp)
