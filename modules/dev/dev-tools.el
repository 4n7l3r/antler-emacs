;;; dev-tools.el --- Project management tools -*- lexical-binding: t -*-
(use-package projectile
  :straight t
  :defer 1
  :bind-keymap
  ("C-c p" . projectile-command-map)
  :custom
  (projectile-completion-system 'default)
  (projectile-enable-caching t)
  (projectile-indexing-method 'alien)
  :config
  (projectile-mode +1))

(setq vc-handled-backends '(Git))
(setq vc-make-backup-files nil)

(use-package vc
  :custom
  (vc-follow-symlinks t)
  (vc-suppress-confirm t)
  :config
  ;; Add error handling for VC processes
  (defun antler/vc-process-sentinel (process _msg)
    "Handle VC process errors gracefully."
    (when (memq (process-status process) '(exit signal))
      (unless (= (process-exit-status process) 0)
        (message "VC process failed with status %d"
                 (process-exit-status process)))))

  (advice-add 'vc-process-sentinel :around
              (lambda (orig-fun &rest args)
                (condition-case err
                    (apply orig-fun args)
                  (error
                   (message "VC Error: %s" (error-message-string err))
                   nil)))))

(use-package magit
  :straight t
  :defer t
  :bind (("C-x g" . magit-status))
  :custom
  (magit-process-finish-apply-ansi-colors t)
  (magit-process-popup-time 10)
  (magit-process-log-max 30)
  :config
  ;; Handle Git process errors
  (defun antler/magit-process-sentinel (process event)
    "Handle Magit process errors gracefully."
    (when (memq (process-status process) '(exit signal))
      (unless (= (process-exit-status process) 0)
        (message "Git process failed: %s" event))))

  (advice-add 'magit-process-sentinel :around
              (lambda (orig-fun &rest args)
                (condition-case err
                    (apply orig-fun args)
                  (error
                   (message "Magit Error: %s" (error-message-string err))
                   nil)))))

(use-package git-modes
  :straight t
  :defer t)

(use-package editorconfig
  :straight t
  :defer 1
  :config
  (editorconfig-mode 1))

(use-package hl-todo
  :straight t
  :hook (prog-mode . hl-todo-mode)
  :custom
  (hl-todo-keyword-faces
   '(("TODO"   . "#FF0000")
     ("FIXME"  . "#FF0000")
     ("DEBUG"  . "#A020F0")
     ("NOTE"   . "#28B463"))))

(use-package command-log-mode
  :straight t
  :defer t
  :commands command-log-mode)

(use-package discover-my-major
  :straight t
  :bind (("C-h C-m" . discover-my-major)))

;; Tree-sitter support for Emacs 29+
(when (and (fboundp 'treesit-available-p)
           (treesit-available-p))
  ;; Set up tree-sitter language sources
  (setq treesit-language-source-alist '())

  ;; Function to install missing grammars
  (defun antler/treesit-install-grammars ()
    "Install or update tree-sitter grammars."
    (interactive)
    (dolist (grammar treesit-language-source-alist)
      (let ((lang (car grammar)))
        (unless (treesit-language-available-p lang)
          (treesit-install-language-grammar lang)))))

  ;; Auto-install missing grammars
  (antler/treesit-install-grammars)
  
  ;; Use treesit-auto if available
  (use-package treesit-auto
    :straight t
    :custom
    (treesit-auto-install 'prompt)
    :config
    (treesit-auto-add-to-auto-mode-alist 'all)
    (global-treesit-auto-mode)))

(provide 'dev-tools)
