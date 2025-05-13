;;; dev-terminal.el --- Terminal integration -*- lexical-binding: t -*-

;; Check if system can build vterm
(defvar vterm-install-ready-p
  (and (not (eq system-type 'windows-nt))
       (or (fboundp 'module-load)
           (null module-file-suffix))))

;; Enable better VTerm terminal - the most modern terminal emulator for Emacs
(use-package vterm
  :straight t
  :when vterm-install-ready-p
  :custom
  (vterm-max-scrollback 10000)
  (vterm-buffer-name-string "vterm %s")
  (vterm-use-vterm-prompt-detection-method t)
  (vterm-kill-buffer-on-exit t))

;; Fallback for systems that can't build vterm
(use-package term
  :when (not vterm-install-ready-p)
  :commands (term ansi-term))

;; Add project integration - allow creating terminals in project root
(use-package vterm-toggle
  :straight t
  :when vterm-install-ready-p
  :bind
  (("C-c v t" . vterm-toggle)
   ("C-c v n" . vterm-toggle-forward)
   ("C-c v p" . vterm-toggle-backward))
  :custom
  (vterm-toggle-fullscreen-p nil)
  (vterm-toggle-scope 'project)
  :config
  (add-to-list 'display-buffer-alist
               '("\\*vterm\\*"
                 (display-buffer-reuse-window display-buffer-in-side-window)
                 (side . bottom)
                 (dedicated . t)
                 (window-height . 0.3))))

;; Determine best available shell
(defun antler/best-available-shell ()
  "Return the best available shell on the system."
  (cond
   ((executable-find "fish") (executable-find "fish"))
   ((executable-find "zsh") (executable-find "zsh"))
   (t (getenv "SHELL"))))

;; Use fish or zsh shell if available, otherwise default shell
(use-package shell
  :custom
  (shell-file-name (antler/best-available-shell))
  (explicit-shell-file-name (antler/best-available-shell)))

;; Better shell experience with native compilation
(use-package eshell
  :defer t
  :init
  (add-hook 'eshell-mode-hook
            (lambda ()
              (eshell-cmpl-initialize)
              (define-key eshell-mode-map (kbd "<tab>") 'completion-at-point)))
  :custom
  (eshell-banner-message "")
  (eshell-scroll-to-bottom-on-input t)
  (eshell-scroll-to-bottom-on-output t)
  (eshell-hist-ignoredups t)
  (eshell-history-size 10000)
  (eshell-buffer-shorthand t)
  (eshell-highlight-prompt t)
  (eshell-prompt-regexp "^[^#$\n]*[#$] ")
  (eshell-prompt-function
   (lambda ()
     (concat
      (if (string= (eshell/pwd) (getenv "HOME"))
          "~"
        (abbreviate-file-name (eshell/pwd)))
      (if (= (user-uid) 0) " # " " $ ")))))

;; Set up temporary and backup file locations
(let ((backup-dir (expand-file-name "~/.cache/emacs/backups"))
      (auto-save-dir (expand-file-name "~/.cache/emacs/auto-save")))
  ;; Create auto-save directory if it doesn't exist
  (unless (file-exists-p auto-save-dir)
    (make-directory auto-save-dir t))
  
  ;; Configure backup settings
  (setq backup-directory-alist `(("." . ,backup-dir))
        auto-save-file-name-transforms `((".*" ,auto-save-dir t))
        backup-by-copying t
        version-control t
        delete-old-versions t
        kept-new-versions 6
        kept-old-versions 2))

;; Tramp for remote access, including container support
(use-package tramp
  :defer t
  :custom
  (tramp-default-method "ssh")
  (tramp-backup-directory-alist backup-directory-alist)
  (tramp-verbose 1)
  (tramp-auto-save-directory
   (expand-file-name "~/.cache/emacs/tramp-autosave"))
  :config
  ;; Create tramp auto-save directory if it doesn't exist
  (unless (file-exists-p tramp-auto-save-directory)
    (make-directory tramp-auto-save-directory t))
  
  ;; Enable container integration (replaces docker-tramp)
  (with-eval-after-load 'tramp
    (require 'tramp-container nil t)
    (when (functionp 'tramp-container-configure)
      (tramp-container-configure)
      (message "Configured tramp-container for Docker access"))))

;; Enhanced Docker integration
(use-package docker
  :straight t
  :bind ("C-c d" . docker)
  :custom
  (docker-command "docker"))

;; Docker Compose mode
(use-package docker-compose-mode
  :straight t
  :mode "docker-compose.*\\.ya?ml\\'")

;; Enable shell command from dired
(with-eval-after-load 'dired
  (define-key dired-mode-map (kbd "!") 'dired-smart-shell-command))

(provide 'dev-terminal) 