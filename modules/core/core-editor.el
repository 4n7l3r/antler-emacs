;;; core-editor.el --- Editor configuration -*- lexical-binding: t -*-

;; Disable line numbers in specific modes
(dolist (mode '(term-mode-hook
                shell-mode-hook
                eshell-mode-hook))
  (add-hook mode (lambda () (display-line-numbers-mode 0))))

(bind-key "S-<return>" 'sanityinc/newline-at-end-of-line)

(when (boundp 'display-fill-column-indicator)
  (setq-default indicate-buffer-boundaries 'left)
  (setq-default display-fill-column-indicator-character ?<)
  (setq-default fill-column 120)
  (add-hook 'prog-mode-hook 'display-fill-column-indicator-mode))

(use-package symbol-overlay
  :ensure t
  :init
  (dolist (hook '(prog-mode-hook html-mode-hook yaml-mode-hook conf-mode-hook))
    (add-hook hook 'symbol-overlay-mode))
  :bind (:map symbol-overlay-mode-map
              ("M-i" . symbol-overlay-put)
              ("M-I" . symbol-overlay-remove-all)
              ("M-n" . symbol-overlay-jump-next)
              ("M-p" . symbol-overlay-jump-prev)))

(use-package desktop
  :custom
  (desktop-restore-frames t)
  (desktop-save t)
  :config
  (desktop-save-mode 1))

;; Save window parameters between sessions
(add-to-list 'desktop-globals-to-save 'default-frame-alist)

(use-package browse-kill-ring
  :ensure t
  :init
  (setq browse-kill-ring-separator "\f")
  :bind (("M-Y" . browse-kill-ring)
         :map browse-kill-ring-mode-map
         ("C-g" . browse-kill-ring-quit)
         ("M-n" . browse-kill-ring-forward)
         ("M-p" . browse-kill-ring-previous)))

(use-package page-break-lines
  :ensure t
  :config
  (add-to-list 'page-break-lines-modes 'browse-kill-ring-mode))

(use-package avy
  :ensure t
  :bind (("C-;" . avy-goto-char-timer)
         ("M-g g" . avy-goto-line)))

(use-package multiple-cursors
  :ensure t
  :bind (("C-<" . mc/mark-previous-like-this)
         ("C->" . mc/mark-next-like-this)
         ("C-+" . mc/mark-next-like-this)
         ("C-c C-<" . mc/mark-all-like-this)))



(use-package highlight-escape-sequences
  :ensure t
  :init
  (add-hook 'after-init-hook 'hes-mode))

(defun sanityinc/disable-features-during-macro-call (orig &rest args)
  "When running a macro, disable features that might be expensive.
ORIG is the advised function, which is called with its ARGS."
  (let (post-command-hook
        font-lock-mode
        (tab-always-indent (or (eq 'complete tab-always-indent) tab-always-indent)))
    (apply orig args)))

(advice-add 'kmacro-call-macro :around 'sanityinc/disable-features-during-macro-call)


;; Nicer naming of buffers for files with identical names
(require 'uniquify)

(setq uniquify-buffer-name-style 'reverse)
(setq uniquify-separator " • ")
(setq uniquify-after-kill-buffer-p t)
(setq uniquify-ignore-buffers-re "^\\*")

(use-package ibuffer
  :bind (("C-x C-b" . ibuffer)
         ("C-x k" . kill-this-buffer))
  :config
  ;; Use human readable Size column instead of original one
  (define-ibuffer-column size-h
    (:name "Size" :inline t)
    (file-size-human-readable (buffer-size)))
  
  ;; Modify the default ibuffer-formats (toggle with `)
  ;; TODO: add formats
  
  (setq ibuffer-filter-group-name-face 'font-lock-doc-face))

(defun ibuffer-set-up-preferred-filters ()
  (ibuffer-vc-set-filter-groups-by-vc-root)
  (unless (eq ibuffer-sorting-mode 'filename/process)
    (ibuffer-do-sort-by-filename/process)))

(use-package ibuffer-vc
  :ensure t
  :init
  (add-hook 'ibuffer-hook 'ibuffer-set-up-preferred-filters)
  (setq-default ibuffer-show-empty-filter-groups nil))



;; Settings for tracking recent files
(add-hook 'after-init-hook 'recentf-mode)

(setq-default
 recentf-max-saved-items 1000
 recentf-exclude `("/tmp/" "/ssh:" ,(concat package-user-dir "/.*-autoloads\\.el\\'")))

(use-package hippie-exp
  :bind (([remap dabbrev-expand] . hippie-expand)
         ("M-/" . hippie-expand))
  :config
  (setq hippie-expand-try-functions-list
        '(try-complete-file-name-partially
          try-complete-file-name
          try-expand-dabbrev
          try-expand-dabbrev-all-buffers
          try-expand-dabbrev-from-kill)))

;; Parentheses
;;(electric-pair-mode 1)
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

(use-package crux
  :ensure t
  :bind
  (("C-a" . crux-move-beginning-of-line)
   ("C-c d" . crux-duplicate-current-line-or-region)
   ("C-c r" . crux-rename-file-and-buffer)
   ("C-c k" . crux-kill-whole-line)))

(use-package smartparens
  :ensure t
  :hook (prog-mode . smartparens-mode)
  :custom
  (sp-base-key-bindings 'paredit)
  (sp-autoskip-closing-pair 'always)
  :config
  (require 'smartparens-config))

(use-package aggressive-indent
  :ensure t
  :hook
  (prog-mode . aggressive-indent-mode)
  :custom
  (aggressive-indent-comments-too t))

(use-package super-save
  :ensure t
  :custom
  (super-save-auto-save-when-idle t)
  (super-save-idle-duration 30)
  :config
  (super-save-mode +1))

(use-package vundo
  :ensure t
  :bind (("C-x u" . vundo))
  :custom
  (vundo-glyph-alist vundo-unicode-symbols)
  (vundo-compact-display t))

(use-package transient
  :ensure t
  :pin gnu  ; Ensure we get it from GNU ELPA
  :custom
  (transient-default-level 5)
  (transient-display-buffer-action '(display-buffer-below-selected))
  (transient-levels-file
   (expand-file-name "transient/levels.el" user-emacs-directory))
  (transient-values-file
   (expand-file-name "transient/values.el" user-emacs-directory))
  (transient-history-file
   (expand-file-name "transient/history.el" user-emacs-directory)))

(use-package list-unicode-display
  :ensure t)

(provide 'core-editor)
