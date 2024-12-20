;;; dev-treemacs.el --- Treemacs configuration -*- lexical-binding: t -*-
(use-package treemacs
  :ensure t
  :defer t
  :custom
  (treemacs-collapse-dirs 3)
  (treemacs-deferred-git-apply-delay 0.5)
  (treemacs-display-in-side-window t)
  (treemacs-file-event-delay 5000)
  (treemacs-file-follow-delay 0.2)
  (treemacs-follow-after-init t)
  (treemacs-git-command-pipe "")
  (treemacs-goto-tag-strategy 'refetch-index)
  (treemacs-indentation 2)
  (treemacs-indentation-string " ")
  (treemacs-max-git-entries 5000)
  (treemacs-missing-project-action 'ask)
  (treemacs-no-png-images nil)
  (treemacs-persist-file (expand-file-name ".cache/treemacs-persist" user-emacs-directory))
  (treemacs-position 'left)
  (treemacs-recenter-distance 0.1)
  (treemacs-recenter-after-file-follow nil)
  (treemacs-recenter-after-tag-follow nil)
  (treemacs-recenter-after-project-jump 'always)
  (treemacs-recenter-after-project-expand 'on-distance)
  (treemacs-show-cursor nil)
  (treemacs-show-hidden-files t)
  (treemacs-silent-filewatch nil)
  (treemacs-silent-refresh nil)
  (treemacs-sorting 'alphabetic-asc)
  (treemacs-space-between-root-nodes t)
  (treemacs-tag-follow-cleanup t)
  (treemacs-tag-follow-delay 1.5)
  :config
  (treemacs-follow-mode t)
  (treemacs-filewatch-mode t)
  (treemacs-fringe-indicator-mode 'always)
  :bind
  (:map global-map
        ("M-0"     . treemacs-select-window)
        ("C-x t 1" . treemacs-delete-other-windows)
        ("C-x t t" . treemacs)
        ("C-x t d" . treemacs-select-directory)
        ("C-x t B" . treemacs-bookmark)
        ("C-x t C-t" . treemacs-find-file)
        ("C-x t M-t" . treemacs-find-tag)))

(add-hook 'treemacs-mode-hook (lambda () (display-line-numbers-mode 0)))

(use-package treemacs-projectile
  :after (treemacs projectile)
  :ensure t)

;; (use-package treemacs-icons-dired
;;   :hook (dired-mode . treemacs-icons-dired-enable-once)
;;   :ensure t)

(use-package treemacs-all-the-icons
  :ensure t
  :after treemacs
  :config
  (treemacs-load-theme "all-the-icons"))

(use-package treemacs-magit
  :after (treemacs magit)
  :ensure t)

(provide 'dev-treemacs)
