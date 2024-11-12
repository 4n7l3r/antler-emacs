;;; core-performance.el --- Performance optimizations -*- lexical-binding: t -*-

;; Garbage Collection
;; Adjust garbage collection threshold for early startup (see use of gcmh below)
(setq gc-cons-threshold (* 128 1024 1024))

;; Process performance tuning
(setq read-process-output-max (* 4 1024 1024))
(setq process-adaptive-read-buffering nil)

;; warn when opening files bigger than 100MB
(setq large-file-warning-threshold 100000000)

;; File handling optimizations
(setq auto-mode-case-fold nil)
(setq create-lockfiles nil)
(setq make-backup-files nil)
(setq auto-save-default nil)

;; Native compilation settings
(when (featurep 'native-compile)
  (setq native-comp-async-report-warnings-errors nil)
  (setq native-comp-deferred-compilation t)
  (setq native-comp-speed 2))

;; Always load newest byte code
(setq load-prefer-newer t)

(provide 'core-performance)
