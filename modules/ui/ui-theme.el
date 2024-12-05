;;; ui-theme.el --- Theme configuration -*- lexical-binding: t -*-

(use-package doom-themes
  :ensure t
  :custom
  (doom-themes-enable-bold t)
  (doom-themes-enable-italic t)
  :config
  (load-theme 'doom-1337 t)

  ;; Enable flashing mode-line on errors
  (doom-themes-visual-bell-config)

  ;; Corrects and improves org-mode's native fontification
  (doom-themes-org-config))

(use-package all-the-icons
  :ensure t
  :if (display-graphic-p))

(use-package all-the-icons-dired
  :ensure t
  :hook (dired-mode . all-the-icons-dired-mode))

(use-package doom-modeline
  :ensure t
  :after all-the-icons
  :init
  (setq doom-modeline-support-imenu t)
  :custom
  (doom-modeline-buffer-encoding t)
  (doom-modeline-buffer-file-name-style 'truncate-upto-project)
  (doom-modeline-buffer-modification-icon t)
  (doom-modeline-buffer-name t)
  (doom-modeline-buffer-state-icon t)
  (doom-modeline-checker-simple-format t)
  (doom-modeline-enable-word-count t)
  (doom-modeline-env-version t)
  (doom-modeline-height 25)
  (doom-modeline-icon t)
  (doom-modeline-indent-info t)
  (doom-modeline-irc-stylize 'identity)
  (doom-modeline-major-mode-color-icon t)
  (doom-modeline-major-mode-icon t)
  (doom-modeline-minor-modes nil)
  (doom-modeline-project-detection 'auto)
  ;;  (doom-modeline-bar-width 3)
  :config
  ;; Run after frame creation to ensure fonts are loaded
  (doom-modeline-mode))

;; Install required fonts
(use-package nerd-icons
  :ensure t)

(provide 'ui-theme)
