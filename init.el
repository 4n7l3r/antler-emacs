;;; init.el --- Load the full configuration -*- lexical-binding: t -*-
;;; Commentary:
;;; Code:

;; Produce backtraces when errors occur: can be helpful to diagnose startup issues
(setq debug-on-error t)

;; Add all module directories to load-path first
(defvar modules-dir (expand-file-name "modules" user-emacs-directory))
(dolist (dir '("core" "completion" "dev" "lang" "org-mode" "ui"))
  (add-to-list 'load-path (expand-file-name dir modules-dir)))

;; Straight.el debugging and error handling
(setq straight-check-for-modifications '(find-when-checking))
(setq straight-vc-git-default-protocol 'https)  ;; Use HTTPS instead of SSH
(setq straight-fix-org nil)  ;; Don't build org-mode twice
(setq straight-disable-native-compile nil) ;; Enable native compilation if available

;; Set up error handling for network issues
(defvar network-retry-count 0)
(defun handle-network-errors (orig-fun &rest args)
  "Handle network errors gracefully in ORIG-FUN with ARGS."
  (condition-case err
      (apply orig-fun args)
    (error
     (if (and (< network-retry-count 3)
              (string-match-p "Failed to run \"git\"" (error-message-string err)))
         (progn
           (setq network-retry-count (1+ network-retry-count))
           (message "Network error, retrying in 2 seconds... (attempt %d/3)" network-retry-count)
           (sit-for 2)
           (apply #'handle-network-errors orig-fun args))
       (signal (car err) (cdr err))))))

(advice-add 'straight--clone-repository :around #'handle-network-errors)

;; Core modules
(require 'core-packages)     ;; Package management
(require 'core-performance)  ;; Performance optimizations

;; Load theme first to ensure UI is consistent
(require 'ui-theme)          ;; Theme configuration

;; Then load other UI components
(require 'core-ui)           ;; Basic UI settings
(require 'core-keybindings)  ;; Global keybindings
(require 'core-navigation)   ;; Navigation helpers
(require 'core-editor)       ;; Editor behavior
(require 'core-search)       ;; Search functionality

;; Completion modules
(require 'completion-corfu)   ;; In-buffer completion
(require 'completion-vertico)  ;; Minibuffer completion
(require 'completion-consult)  ;; Enhanced commands

;; Development modules
(require 'dev-lsp)        ;; Language server protocol
(require 'dev-tools)      ;; Development tools
(require 'dev-treemacs)   ;; File tree
(require 'dev-debug)      ;; Debugging support
(require 'dev-project)    ;; Project management
(require 'dev-terminal)   ;; Terminal integration

;; Language modules - uncomment as needed
(require 'lang-elisp)     ;; Emacs Lisp
(require 'lang-go)        ;; Go
(when (locate-library "modules/lang/lang-rust")
  (require 'lang-rust))   ;; Rust (conditionally loaded)
(when (locate-library "modules/lang/lang-python")
  (require 'lang-python)) ;; Python (conditionally loaded)
(when (locate-library "modules/lang/lang-typescript")
  (require 'lang-typescript)) ;; TypeScript (conditionally loaded)

;; ORG Mode
(require 'org-mode)
(require 'org-hypothesis)

;; UI modules - load last for consistent appearance
(require 'ui-modeline)   ;; Mode line configuration

;; Load personal customizations if they exist
(when (file-exists-p (expand-file-name "personal.el" modules-dir))
  (add-to-list 'load-path modules-dir)
  (require 'personal))

;; Restore normal GC threshold after startup is complete
(add-hook 'emacs-startup-hook
          (lambda ()
            (setq gc-cons-threshold (* 20 1024 1024))
            (when (boundp 'gcmh-high-cons-threshold)
              (setq gcmh-high-cons-threshold (* 20 1024 1024)))
            (message "Emacs ready in %s with %d garbage collections."
                     (format "%.2f seconds"
                             (float-time
                              (time-subtract after-init-time before-init-time)))
                     gcs-done)))

;; Disable debug-on-error after startup
(add-hook 'emacs-startup-hook
          (lambda ()
            (setq debug-on-error nil)))

(provide 'init)

(custom-set-variables
 ;; custom-set-variables was added by Custom.
 ;; If you edit it by hand, you could mess it up, so be careful.
 ;; Your init file should contain only one such instance.
 ;; If there is more than one, they won't work right.
 '(custom-safe-themes
   '("d481904809c509641a1a1f1b1eb80b94c58c210145effc2631c1a7f2e4a2fdf4"
     "4b6cc3b60871e2f4f9a026a5c86df27905fb1b0e96277ff18a76a39ca53b82e1"
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
