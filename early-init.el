;;; early-init.el --- Early Init File -*- lexical-binding: t -*-

;; Performance optimization - these settings help with faster startup
(setq gc-cons-threshold most-positive-fixnum)  ;; Maximum threshold during startup
(setq package-enable-at-startup nil)
(setq load-prefer-newer t)

;; Prevent package.el from modifying the init
(setq package-quickstart nil)
(setq straight-use-package-by-default t)

;; Faster to disable these here in early-init
(push '(menu-bar-lines . 0) default-frame-alist)
(push '(tool-bar-lines . 0) default-frame-alist)
(push '(vertical-scroll-bars) default-frame-alist)
(setq frame-inhibit-implied-resize t)
(setq inhibit-startup-message t)
(setq inhibit-splash-screen t)
(setq inhibit-startup-buffer-menu t)

;; Prevent the glimpse of un-styled Emacs by disabling these UI elements early
(setq tool-bar-mode nil
      menu-bar-mode nil 
      scroll-bar-mode nil)

;; Improve file I/O performance
(setq process-adaptive-read-buffering nil)
(setq read-process-output-max (* 4 1024 1024)) ;; 4MB

;; Module file handling optimization
(when (boundp 'module-file-suffix)
  (setq native-comp-enable-subr-trampolines nil))

;; Disable UI bidirectional text scanning support
(setq-default bidi-display-reordering nil)
(setq bidi-inhibit-bpa t)
(setq-default cursor-in-non-selected-windows nil)

(provide 'early-init)
