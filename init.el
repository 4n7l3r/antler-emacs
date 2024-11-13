;;; init.el --- Load the full configuration -*- lexical-binding: t -*-
;;; Commentary:
;;; Code:

;; Produce backtraces when errors occur: can be helpful to diagnose startup issues
(setq debug-on-error t)

;; Load path setup
(defvar modules-dir (expand-file-name "modules" user-emacs-directory))
(dolist (dir '("core" "completion" "dev" "lang" "org-mode" "ui"))
  (add-to-list 'load-path (expand-file-name dir modules-dir)))

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

;; ORG Mode
(require 'org-mode)

;; UI modules
(require 'ui-theme)
;; (require 'ui-modeline)

(provide 'init)

;; Local Variables:
;; coding: utf-8
;; no-byte-compile: t
;; End:
;;; init.el ends here
