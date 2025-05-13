;;; ui-modeline.el --- Modeline configuration -*- lexical-binding: t -*-

;;; Commentary:
;; Clean implementation of doom-modeline with proper loading and fallbacks

;;; Code:

;; Ensure we have the necessary icon fonts
(use-package nerd-icons
  :straight t
  :custom
  ;; Use fallback fonts that are more likely to be available
  (nerd-icons-font-family (cond
                          ((member "Symbols Nerd Font Mono" (font-family-list))
                           "Symbols Nerd Font Mono")
                          ((member "JetBrainsMono Nerd Font" (font-family-list))
                           "JetBrainsMono Nerd Font")
                          (t "Monospace")))
  :config
  ;; Create a proper persistent flag to skip future font checks
  (defvar my/nerd-icons-skip-font-check
    (locate-user-emacs-file ".nerd-icons-skip-font-check")
    "File indicating that we should skip font installation checks.")
  
  ;; Install fonts only if explicitly requested
  (defun my/install-nerd-fonts ()
    "Install Nerd Fonts manually without attempting auto-installation."
    (interactive)
    (if (nerd-icons-install-fonts t)
        (progn
          (message "Nerd Font installation requested. After installation completes, restart Emacs.")
          (with-temp-file my/nerd-icons-skip-font-check
            (insert "Font installation was attempted on: "
                    (format-time-string "%Y-%m-%d %H:%M:%S" (current-time)))))
      (message "Font installation failed or was cancelled.")))
  
  ;; Manual font check that bypasses the automatic installer
  (unless (or (file-exists-p my/nerd-icons-skip-font-check)
              (member "Symbols Nerd Font Mono" (font-family-list))
              (member "JetBrainsMono Nerd Font" (font-family-list)))
    ;; Show a message without forcing installation
    (message "Nerd Fonts not detected. Run M-x my/install-nerd-fonts to install them.")
    ;; Create the skip file to avoid future messages
    (with-temp-file my/nerd-icons-skip-font-check
      (insert "Skipping font checks as of: "
              (format-time-string "%Y-%m-%d %H:%M:%S" (current-time)))))
  
  ;; Force nerd-icons to use text fallbacks when necessary
  (setq nerd-icons-font-family (cond ((member "Symbols Nerd Font Mono" (font-family-list))
                                     "Symbols Nerd Font Mono")
                                    ((member "JetBrainsMono Nerd Font" (font-family-list))
                                     "JetBrainsMono Nerd Font")
                                    (t "Monospace"))))

;; Doom modeline with careful loading
(use-package doom-modeline
  :straight t
  :after nerd-icons
  :hook (after-init . doom-modeline-mode)
  :custom
  ;; Basic appearance
  (doom-modeline-height 25)
  (doom-modeline-bar-width 3)
  (doom-modeline-hud nil)
  
  ;; Icon settings
  (doom-modeline-icon t)
  (doom-modeline-major-mode-icon t)
  (doom-modeline-major-mode-color-icon t)
  
  ;; Buffer information
  (doom-modeline-buffer-file-name-style 'truncate-upto-project)
  (doom-modeline-buffer-state-icon t)
  (doom-modeline-buffer-modification-icon t)
  
  ;; Content indicators
  (doom-modeline-buffer-encoding t)
  (doom-modeline-checker-simple-format t)
  (doom-modeline-indent-info nil)
  (doom-modeline-number-limit 99)
  (doom-modeline-percent-position nil)
  
  ;; VCS
  (doom-modeline-vcs-max-length 12)
  
  ;; Project features
  (doom-modeline-workspace-name t)
  (doom-modeline-persp-name t)
  (doom-modeline-persp-icon t)
  
  ;; Misc features to enable
  (doom-modeline-env-version t)
  (doom-modeline-lsp t)
  (doom-modeline-modal-icon t)
  
  ;; Features to disable to improve performance
  (doom-modeline-minor-modes nil)
  (doom-modeline-enable-word-count nil)
  (doom-modeline-continuous-word-count-modes '(markdown-mode gfm-mode org-mode))
  (doom-modeline-github nil)
  (doom-modeline-mu4e nil)
  (doom-modeline-irc nil)
  (doom-modeline-debug nil)
  
  :init
  ;; Set initial values with error handling
  (unless after-init-time
    (setq-default mode-line-format nil))
  
  :config
  ;; Ensure theme compatibility
  (with-eval-after-load 'doom-themes
    (let ((faces '(doom-modeline-buffer-path
                  doom-modeline-buffer-file
                  doom-modeline-buffer-modified
                  doom-modeline-project-dir)))
      (dolist (face faces)
        (when (facep face)
          (set-face-attribute face nil :inherit nil)))))

  ;; Add fallback for missing icons
  (defun my/safe-nerd-icons-icon-for-file (file)
    "Safely get icon for FILE. Avoid errors if icon functions unavailable."
    (ignore-errors (nerd-icons-icon-for-file file)))
  
  (advice-add #'doom-modeline-update-vcs-text :around
              (lambda (orig-fun &rest args)
                (if (fboundp 'vc-git--out-ok)
                    (apply orig-fun args)
                  ""))))

;; Tab bar mode configuration
(use-package tab-bar
  :custom
  (tab-bar-show 1)
  (tab-bar-format '(tab-bar-format-history tab-bar-format-tabs tab-bar-separator tab-bar-format-align-right tab-bar-format-global))
  (tab-bar-tab-hints t)
  (tab-bar-select-tab-modifiers '(meta))
  (tab-bar-new-tab-choice "*scratch*")
  (tab-bar-close-button-show nil)
  (tab-bar-new-button-show nil)
  :config
  (tab-bar-mode 1)
  (tab-bar-history-mode 1))

;; Display dividers between windows
(setq window-divider-default-places t
      window-divider-default-bottom-width 1
      window-divider-default-right-width 1)
(window-divider-mode 1)

;; Text scaling
(use-package default-text-scale
  :straight t
  :bind (("C-M-=" . default-text-scale-increase)
         ("C-M--" . default-text-scale-decrease)
         ("C-M-0" . default-text-scale-reset))
  :config
  (default-text-scale-mode 1))

;; Fallback to default modeline if doom-modeline fails to load
(with-eval-after-load 'doom-modeline
  (unless (fboundp 'doom-modeline-mode)
    (message "Doom-modeline failed to load properly, using standard modeline")
    (setq-default mode-line-format
                 '("%e"
                   mode-line-front-space
                   mode-line-mule-info
                   mode-line-client
                   mode-line-modified
                   mode-line-remote
                   mode-line-frame-identification
                   mode-line-buffer-identification
                   "   "
                   mode-line-position
                   (vc-mode vc-mode)
                   "  "
                   mode-line-modes
                   mode-line-misc-info
                   mode-line-end-spaces))))

(provide 'ui-modeline)
;;; ui-modeline.el ends here