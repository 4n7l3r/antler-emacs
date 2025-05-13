;;; core-packages.el --- Package management -*- lexical-binding: t -*-
(require 'cl-lib)

;; Initialize straight.el
(defvar bootstrap-version)
(let ((bootstrap-file
       (expand-file-name "straight/repos/straight.el/bootstrap.el" user-emacs-directory))
      (bootstrap-version 6))
  (unless (file-exists-p bootstrap-file)
    (with-current-buffer
        (url-retrieve-synchronously
         "https://raw.githubusercontent.com/radian-software/straight.el/develop/install.el"
         'silent 'inhibit-cookies)
      (goto-char (point-max))
      (eval-print-last-sexp)))
  (load bootstrap-file nil 'nomessage))

;; Configure straight.el
(setq straight-use-package-by-default t)
(setq straight-check-for-modifications '(check-on-save find-when-checking))
(setq straight-vc-git-default-clone-depth 1)  ;; Shallow clone for faster downloads

;; Use use-package with straight.el
(straight-use-package 'use-package)
(setq use-package-always-ensure nil)  ;; Not needed with straight.el
(setq use-package-always-defer nil)   ;; Don't defer by default
(setq use-package-expand-minimally t) ;; Performance optimization
(setq use-package-compute-statistics t) ;; For M-x use-package-report

;; Initialize package archives
(setq package-archives '(("gnu" . "https://elpa.gnu.org/packages/")
                        ("melpa" . "https:/q/melpa.org/packages/")
                        ("org" . "https://orgmode.org/elpa/")))

;; Check if directories exist and create them if needed
(let ((dirs '("~/org" "~/org/roam" "~/.cache/emacs/backups")))
  (dolist (dir dirs)
    (let ((dir-path (expand-file-name dir)))
      (unless (file-exists-p dir-path)
        (make-directory dir-path t)))))

;; Enable improved package management for built-in packages
(setq package-install-upgrade-built-in t)

(provide 'core-packages)
