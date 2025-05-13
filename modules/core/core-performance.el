;;; core-performance.el --- Performance optimizations -*- lexical-binding: t -*-

;; System type detection for platform-specific optimizations
(defconst *is-mac* (eq system-type 'darwin))
(defconst *is-linux* (eq system-type 'gnu/linux))
(defconst *is-windows* (or (eq system-type 'ms-dos) (eq system-type 'windows-nt)))
(defconst *is-bsd* (or (eq system-type 'berkeley-unix) (eq system-type 'darwin)))

;; Optimizations for macOS
(when *is-mac*
  ;; Enable pixel-based scrolling
  (when (boundp 'pixel-scroll-precision-mode)
    (pixel-scroll-precision-mode t))
  ;; Use the Mac high-resolution emoji font
  (set-fontset-font t 'symbol (font-spec :family "Apple Color Emoji") nil 'prepend)
  ;; macOS GUI optimization
  (when (display-graphic-p)
    (setq frame-resize-pixelwise t)
    (add-to-list 'default-frame-alist '(ns-transparent-titlebar . t))
    (add-to-list 'default-frame-alist '(ns-appearance . dark))))

;; Linux-specific optimizations
(when *is-linux*
  ;; Enable double buffering for better rendering
  (when (display-graphic-p)
    (setq x-wait-for-event-timeout nil)
    (setq x-double-buffer t))
  ;; Use Emoji font on Linux if available
  (when (member "Noto Color Emoji" (font-family-list))
    (set-fontset-font t 'symbol (font-spec :family "Noto Color Emoji") nil 'prepend)))

;; Garbage Collection - using gcmh for better management
(use-package gcmh
  :straight t
  :diminish gcmh-mode
  :custom
  (gcmh-idle-delay 5)  ;; Collect garbage when idle for 5 seconds
  (gcmh-high-cons-threshold (* 32 1024 1024))  ;; 32MB when idle
  (gcmh-low-cons-threshold (* 16 1024 1024))   ;; 16MB when in use
  :config
  ;; Extra optimization during minibuffer usage
  (add-hook 'minibuffer-setup-hook
            (lambda ()
              (setq gc-cons-threshold most-positive-fixnum)))
  (add-hook 'minibuffer-exit-hook
            (lambda ()
              (setq gc-cons-threshold gcmh-high-cons-threshold)))
  (gcmh-mode 1))

;; Process performance tuning
(setq read-process-output-max (* 4 1024 1024)) ;; 4MB
(setq process-adaptive-read-buffering nil)

;; IO performance tuning
(setq confirm-kill-processes nil)  ;; Don't ask about running processes when quitting
(setq remote-file-name-inhibit-cache nil) ;; Better remote file caching
(setq create-lockfiles nil)  ;; Don't create .# lock files
(setq auto-save-file-name-transforms
      `((".*" ,(concat (expand-file-name "~/.cache/emacs/auto-save/") "\\1") t)))

;; Warn when opening files bigger than 100MB
(setq large-file-warning-threshold 100000000)

;; File handling optimizations
(setq auto-mode-case-fold nil)
(setq make-backup-files t)
(setq backup-by-copying t)
(setq version-control t)
(setq delete-old-versions t)
(setq kept-new-versions 6)
(setq kept-old-versions 2)
(setq backup-directory-alist `(("." . ,(expand-file-name "~/.cache/emacs/backups"))))
(setq vc-make-backup-files nil)
(setq auto-save-list-file-prefix (expand-file-name "~/.cache/emacs/auto-save/.saves-"))

;; Native compilation settings for Emacs 29+
(when (featurep 'native-compile)
  (setq native-comp-async-report-warnings-errors 'silent) ;; Silence warnings
  (setq native-comp-deferred-compilation t)
  (setq native-comp-speed 2)
  (setq native-comp-async-jobs-number (max 2 (- (num-processors) 1)))) ;; Use N-1 cores

;; For Emacs 31 - Tree sitter optimization
(when (>= emacs-major-version 29)
  (setq treesit-extra-load-path
        (list (expand-file-name "tree-sitter" user-emacs-directory))))

;; Always load newest byte code
(setq load-prefer-newer t)

;; Use jit-lock for better font-lock performance
(setq jit-lock-defer-time 0.05
      jit-lock-stealth-time 1
      jit-lock-stealth-nice 0.1
      jit-lock-stealth-verbose nil)

;; Improve minibuffer responsiveness 
(setq echo-keystrokes 0.01)

;; Improve pixel line scrolling performance
(setq pixel-scroll-precision-use-momentum t)

;; Improve general scrolling
(setq fast-but-imprecise-scrolling t)
(setq redisplay-skip-fontification-on-input t)

;; File notification optimization
(when (fboundp 'filenotify-add-watch)
  (setq file-notify-descriptors (make-hash-table :test 'equal))
  (setq file-notify-timeout 10))

;; Allow some functions to be executed without confirming to speed up workflow
(setq enable-local-variables :safe)
(put 'narrow-to-region 'disabled nil)
(put 'narrow-to-page 'disabled nil)
(put 'narrow-to-defun 'disabled nil)
(put 'upcase-region 'disabled nil)
(put 'downcase-region 'disabled nil)
(put 'erase-buffer 'disabled nil)

(provide 'core-performance)
