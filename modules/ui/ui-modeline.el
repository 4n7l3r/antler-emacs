;;; ui-modeline.el --- Modeline configuration -*- lexical-binding: t -*-

(use-package doom-modeline
  :ensure t
  :after all-the-icons
  :custom
  (doom-modeline-height 25)
  (doom-modeline-bar-width 3)
  (doom-modeline-icon t)
  (doom-modeline-major-mode-icon t)
  (doom-modeline-major-mode-color-icon t)
  (doom-modeline-buffer-file-name-style 'truncate-upto-project)
  (doom-modeline-buffer-state-icon t)
  (doom-modeline-buffer-modification-icon t)
  (doom-modeline-minor-modes nil)
  (doom-modeline-enable-word-count nil)
  (doom-modeline-buffer-encoding t)
  (doom-modeline-indent-info nil)
  (doom-modeline-checker-simple-format t)
  (doom-modeline-vcs-max-length 12)
  (doom-modeline-env-version t)
  (doom-modeline-irc-stylize 'identity)
  (doom-modeline-github-timer nil)
  :config
  ;; Run after frame creation to ensure fonts are loaded
  (add-hook 'after-init-hook #'doom-modeline-mode))

;; Install required fonts
(use-package nerd-icons
  :ensure t
  :custom
  (nerd-icons-font-family "JetBrainsMono Nerd Font")
  :config
  (when (and (display-graphic-p)
             (not (member "JetBrainsMono Nerd Font" (font-family-list))))
    (nerd-icons-install-fonts t)))

(provide 'ui-modeline)