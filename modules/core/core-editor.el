;;; core-editor.el --- Editor behavior -*- lexical-binding: t -*-

;; Configure Tree-sitter for modern syntax handling
(when (and (fboundp 'treesit-available-p)
           (treesit-available-p))
  ;; Preferred language modes
  (setq major-mode-remap-alist
        '((c-mode . c-ts-mode)
          (c++-mode . c++-ts-mode)
          (cmake-mode . cmake-ts-mode)
          (conf-toml-mode . toml-ts-mode)
          (css-mode . css-ts-mode)
          (js-mode . js-ts-mode)
          (java-mode . java-ts-mode)
          (json-mode . json-ts-mode)
          (python-mode . python-ts-mode)
          (ruby-mode . ruby-ts-mode)
          (rust-mode . rust-ts-mode)
          (typescript-mode . typescript-ts-mode)
          (tsx-mode . tsx-ts-mode)
          (yaml-mode . yaml-ts-mode)))
  
  ;; Install missing tree-sitter grammars automatically
  (setq treesit-language-source-alist
        '((bash "https://github.com/tree-sitter/tree-sitter-bash")
          (c "https://github.com/tree-sitter/tree-sitter-c")
          (cpp "https://github.com/tree-sitter/tree-sitter-cpp")
          (css "https://github.com/tree-sitter/tree-sitter-css")
          (go "https://github.com/tree-sitter/tree-sitter-go")
          (html "https://github.com/tree-sitter/tree-sitter-html")
          (javascript "https://github.com/tree-sitter/tree-sitter-javascript")
          (json "https://github.com/tree-sitter/tree-sitter-json")
          (lua "https://github.com/tree-sitter/tree-sitter-lua")
          (make "https://github.com/tree-sitter/tree-sitter-make")
          (ocaml "https://github.com/tree-sitter/tree-sitter-ocaml")
          (python "https://github.com/tree-sitter/tree-sitter-python")
          (php "https://github.com/tree-sitter/tree-sitter-php")
          (ruby "https://github.com/tree-sitter/tree-sitter-ruby")
          (rust "https://github.com/tree-sitter/tree-sitter-rust")
          (toml "https://github.com/tree-sitter/tree-sitter-toml")
          (tsx "https://github.com/tree-sitter/tree-sitter-typescript" "master" "tsx/src")
          (typescript "https://github.com/tree-sitter/tree-sitter-typescript" "master" "typescript/src")
          (yaml "https://github.com/tree-sitter/tree-sitter-yaml")))
  
  ;; Configure tree-sitter for better highlighting
  (setq treesit-font-lock-level 4)
  
  ;; Auto-install requested parsers when needed
  (defun antler/auto-install-treesit-if-needed (language)
    "Auto-install the tree-sitter grammar for LANGUAGE if needed."
    (unless (treesit-language-available-p language)
      (message "Installing tree-sitter grammar for %s" language)
      (treesit-install-language-grammar language)))
  
  ;; Install some common languages by default
  (dolist (lang '(bash c cpp css go html javascript json python))
    (antler/auto-install-treesit-if-needed lang)))

;; Disable line numbers in specific modes
(dolist (mode '(term-mode-hook
                shell-mode-hook
                eshell-mode-hook))
  (add-hook mode (lambda () (display-line-numbers-mode 0))))

(global-set-key (kbd "S-<return>") 'sanityinc/newline-at-end-of-line)

(when (boundp 'display-fill-column-indicator)
  (setq-default indicate-buffer-boundaries 'left)
  (setq-default display-fill-column-indicator-character ?<)
  (setq-default fill-column 120)
  (add-hook 'prog-mode-hook 'display-fill-column-indicator-mode))

(use-package symbol-overlay
  :straight t
  :init
  (dolist (hook '(prog-mode-hook html-mode-hook yaml-mode-hook conf-mode-hook))
    (add-hook hook 'symbol-overlay-mode))
  :config
  (define-key symbol-overlay-mode-map (kbd "M-i") 'symbol-overlay-put)
  (define-key symbol-overlay-mode-map (kbd "M-I") 'symbol-overlay-remove-all)
  (define-key symbol-overlay-mode-map (kbd "M-n") 'symbol-overlay-jump-next)
  (define-key symbol-overlay-mode-map (kbd "M-p") 'symbol-overlay-jump-prev))

;; Configure session persistence
(use-package desktop
  :custom
  (desktop-restore-frames t)
  (desktop-save t)
  (desktop-globals-to-save '(desktop-missing-file-warning
                             search-ring
                             regexp-search-ring
                             register-alist
                             file-name-history))
  (desktop-files-not-to-save "^$")
  (desktop-auto-save-timeout 300)
  :config
  (desktop-save-mode 1))

;; Save window parameters between sessions
(add-to-list 'desktop-globals-to-save 'default-frame-alist)

;; Better kill ring
(use-package browse-kill-ring
  :straight t
  :init
  (setq browse-kill-ring-separator "\f")
  (global-set-key (kbd "M-Y") 'browse-kill-ring)
  :config
  (define-key browse-kill-ring-mode-map (kbd "C-g") 'browse-kill-ring-quit)
  (define-key browse-kill-ring-mode-map (kbd "M-n") 'browse-kill-ring-forward)
  (define-key browse-kill-ring-mode-map (kbd "M-p") 'browse-kill-ring-previous))

;; Page breaks
(use-package page-break-lines
  :straight t
  :config
  (add-to-list 'page-break-lines-modes 'browse-kill-ring-mode))

;; Jump to visible text
(use-package avy
  :straight t
  :init
  (global-set-key (kbd "C-;") 'avy-goto-char-timer))

;; Multiple cursors
(use-package multiple-cursors
  :straight t
  :init
  (global-set-key (kbd "C-<") 'mc/mark-previous-like-this)
  (global-set-key (kbd "C->") 'mc/mark-next-like-this)
  (global-set-key (kbd "C-+") 'mc/mark-next-like-this)
  (global-set-key (kbd "C-c C-<") 'mc/mark-all-like-this))

;; Highlight escape sequences
(use-package highlight-escape-sequences
  :straight t
  :init
  (add-hook 'after-init-hook 'hes-mode))

;; Disable features during macros for speed
(defun sanityinc/disable-features-during-macro-call (orig &rest args)
  "When running a macro, disable features that might be expensive.
ORIG is the advised function, which is called with its ARGS."
  (let (post-command-hook
        font-lock-mode
        (tab-always-indent (or (eq 'complete tab-always-indent) tab-always-indent)))
    (apply orig args)))

(advice-add 'kmacro-call-macro :around 'sanityinc/disable-features-during-macro-call)

;; Better buffer naming
(require 'uniquify)
(setq uniquify-buffer-name-style 'reverse)
(setq uniquify-separator " • ")
(setq uniquify-after-kill-buffer-p t)
(setq uniquify-ignore-buffers-re "^\\*")

;; Improve ibuffer
(defun ibuffer-set-up-preferred-filters ()
  (ibuffer-vc-set-filter-groups-by-vc-root)
  (unless (eq ibuffer-sorting-mode 'filename/process)
    (ibuffer-do-sort-by-filename/process)))

(use-package ibuffer-vc
  :straight t
  :init
  (add-hook 'ibuffer-hook 'ibuffer-set-up-preferred-filters)
  (setq-default ibuffer-show-empty-filter-groups nil))

(with-eval-after-load 'ibuffer
  ;; Use human readable Size column instead of original one
  (define-ibuffer-column size-h
    (:name "Size" :inline t)
    (file-size-human-readable (buffer-size))))

;; Buffer/file settings
(setq ibuffer-filter-group-name-face 'font-lock-doc-face)
(setq-default
 recentf-max-saved-items 1000
 recentf-exclude `("/tmp/" "/ssh:" ,(concat package-user-dir "/.*-autoloads\\.el\\'")))
(add-hook 'after-init-hook 'recentf-mode)

;; Better text expansion
(setq hippie-expand-try-functions-list
      '(try-complete-file-name-partially
        try-complete-file-name
        try-expand-dabbrev
        try-expand-dabbrev-all-buffers
        try-expand-dabbrev-from-kill))

;; Parentheses
(show-paren-mode 1)
(setq show-paren-delay 0.1)

;; Whitespace
(setq-default show-trailing-whitespace t)
(add-hook 'before-save-hook 'delete-trailing-whitespace)

;; Save place in files
(save-place-mode 1)

;; Recent files
(recentf-mode 1)
(setq recentf-max-saved-items 100)
(setq recentf-max-menu-items 15)

;; Better scrolling
(setq scroll-margin 0
      scroll-conservatively 100000
      scroll-preserve-screen-position 1)

;; Pixel scroll precision (Emacs 29+)
(when (fboundp 'pixel-scroll-precision-mode)
  (pixel-scroll-precision-mode t))

;; Helpful utilities
(use-package crux
  :straight t
  :bind
  (("C-a" . crux-move-beginning-of-line)
   ("C-c d" . crux-duplicate-current-line-or-region)
   ("C-c r" . crux-rename-file-and-buffer)
   ("C-c k" . crux-kill-whole-line)))

;; Smart parentheses
(use-package smartparens
  :straight t
  :hook (prog-mode . smartparens-mode)
  :custom
  (sp-base-key-bindings 'paredit)
  (sp-autoskip-closing-pair 'always)
  :config
  (require 'smartparens-config))

;; Auto indentation
(use-package aggressive-indent
  :straight t
  :hook
  (prog-mode . aggressive-indent-mode)
  :custom
  (aggressive-indent-comments-too t))

;; Auto save when idle
(use-package super-save
  :straight t
  :custom
  (super-save-auto-save-when-idle t)
  (super-save-idle-duration 30)
  :config
  (super-save-mode +1))

;; Visual undo
(use-package vundo
  :straight t
  :bind (("C-x u" . vundo))
  :custom
  (vundo-glyph-alist vundo-unicode-symbols)
  (vundo-compact-display t))

;; Better transient-mark-mode
(use-package transient
  :straight t
  :custom
  (transient-default-level 5)
  (transient-display-buffer-action '(display-buffer-below-selected))
  (transient-levels-file
   (expand-file-name "transient/levels.el" user-emacs-directory))
  (transient-values-file
   (expand-file-name "transient/values.el" user-emacs-directory))
  (transient-history-file
   (expand-file-name "transient/history.el" user-emacs-directory)))

;; Unicode display
(use-package list-unicode-display
  :straight t)

;; Modern file notification library
(when (fboundp 'filenotify-add-watch)
  (setq file-notify-descriptors (make-hash-table :test 'equal))
  (setq file-notify-timeout 10))

(provide 'core-editor)
