;;; early-init.el --- Early Init File -*- lexical-binding: t -*-

;; Performance optimization
(setq gc-cons-threshold most-positive-fixnum)
(setq package-enable-at-startup nil)
(setq load-prefer-newer t)

;; UI configuration
(push '(menu-bar-lines . 0) default-frame-alist)
(push '(tool-bar-lines . 0) default-frame-alist)
(push '(vertical-scroll-bars) default-frame-alist)
(setq frame-inhibit-implied-resize t)
(setq inhibit-startup-message t)

(provide 'early-init)
