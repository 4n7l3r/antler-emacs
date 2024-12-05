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

(custom-set-variables
 ;; custom-set-variables was added by Custom.
 ;; If you edit it by hand, you could mess it up, so be careful.
 ;; Your init file should contain only one such instance.
 ;; If there is more than one, they won't work right.
 '(custom-safe-themes
   '("4b6cc3b60871e2f4f9a026a5c86df27905fb1b0e96277ff18a76a39ca53b82e1"
     "7c28419e963b04bf7ad14f3d8f6655c078de75e4944843ef9522dbecfcd8717d"
     "4594d6b9753691142f02e67b8eb0fda7d12f6cc9f1299a49b819312d6addad1d"
     "ffafb0e9f63935183713b204c11d22225008559fa62133a69848835f4f4a758c"
     "7964b513f8a2bb14803e717e0ac0123f100fb92160dcf4a467f530868ebaae3e"
     "dd4582661a1c6b865a33b89312c97a13a3885dc95992e2e5fc57456b4c545176"
     "014cb63097fc7dbda3edf53eb09802237961cbb4c9e9abd705f23b86511b0a69" default))
 '(doom-modeline-check-simple-format t nil nil "Customized with use-package doom-modeline"))
(custom-set-faces
 ;; custom-set-faces was added by Custom.
 ;; If you edit it by hand, you could mess it up, so be careful.
 ;; Your init file should contain only one such instance.
 ;; If there is more than one, they won't work right.
 )
 ;; Local Variables:
;; coding: utf-8
;; no-byte-compile: t
;; End:
;;; init.el ends here
