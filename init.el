;;; init.el --- Load the full configuration -*- lexical-binding: t -*-
;;; Commentary:
;;; Code:

;; Produce backtraces when errors occur: can be helpful to diagnose startup issues
(setq debug-on-error t)

;; Add all module directories to load-path first
(defvar modules-dir (expand-file-name "modules" user-emacs-directory))
(dolist (dir '("core" "completion" "dev" "lang" "org-mode" "ui"))
  (add-to-list 'load-path (expand-file-name dir modules-dir)))

;; Ensure elpaca directories exist before loading core-packages
(defvar elpaca-directory (expand-file-name "elpaca/" user-emacs-directory))
(defvar elpaca-builds-directory (expand-file-name "builds/" elpaca-directory))
(defvar elpaca-repos-directory (expand-file-name "repos/" elpaca-directory))

;; Create necessary directories
(dolist (dir (list elpaca-directory elpaca-builds-directory elpaca-repos-directory))
  (unless (file-exists-p dir)
    (make-directory dir t)))

;; Core modules
(require 'core-packages)
(require 'core-performance)
(require 'core-ui)
(require 'core-keybindings)
(require 'core-navigation)
(require 'core-editor)
(require 'core-search)

;; Completion modules
(require 'completion-corfu)
(require 'completion-vertico)
(require 'completion-consult)

;; Development modules
(require 'dev-lsp)
(require 'dev-tools)
(require 'dev-treemacs)
(require 'dev-debug)

;; Language modules
(require 'lang-elisp)
(require 'lang-go)
;;(require 'lang-rust)

;; ORG Mode
(require 'org-mode)
(require 'org-hypothesis)

;; UI modules
(require 'ui-theme)
;; (require 'ui-modeline)

(provide 'init)

;; Local Variables:
;; coding: utf-8
;; no-byte-compile: t
;; End:
;;; init.el ends here
