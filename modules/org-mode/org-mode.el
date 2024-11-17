;;; org-mode.el --- Org mode configuration -*- lexical-binding: t -*-

(defcustom antler/org-directory (concat (getenv "HOME") "/org")
  "Base directory for org files."
  :type 'directory
  :group 'antler)

(defcustom antler/org-roam-directory (concat antler/org-directory "/roam")
  "Base directory for org files."
  :type 'directory
  :group 'antler)

(defun antler/org-remap-move-keys ()
  "Disable M-<arrow> keybindings in Org mode for list manipulation."
  (let ((keys '("<M-up>" "<M-down>" "<M-left>" "<M-right>")))
    (dolist (key keys)
      (define-key org-mode-map (kbd key) nil))))

(require 'org-hypothesis)

(use-package org
  :hook ((org-mode . org-indent-mode)
	 (org-mode . visual-line-mode)
	 (org-mode . variable-pitch-mode)
	 (org-mode . antler/org-remap-move-keys)
	 (org-mode . org-hypothesis-mode)
	 (org-mode . (lambda()
		       (auto-fill-mode 1)
		       (setq fill-column 180))))
  :config
  (setq org-directory antler/org-directory)
  (setq org-default-notes-file (concat antler/org-directory "/notes.org"))
  (setq org-agenda-files (list
			  (concat antler/org-directory "tasks")
			  (concat antler/org-directory "roam")
			  antler/org-directory))
  (setq org-log-done 'time)
  (setq org-return-follows-link t)
  (setq org-hide-emphasis-markers t)
  (add-to-list 'auto-mode-alist '("\\.org\\'" . org-mode))
  (add-hook 'org-mode-hook 'org-indent-mode)
  (org-babel-do-load-languages
   'org-babel-load-languages
   (seq-filter
    (lambda (pair)
      (locate-library (concat "ob-" (symbol-name (car pair)))))
    '((R . t)
      (awk . t)
      (C . t)
      (cpp . t)
      (calc . t)
      (shell . t)
      (clojure . t)
      (css . t)
      (eshell . t)
      (gnuplot . t)
      (sed . t)
      (dot . t)
      (java . t)
      (julia . t)
      (latex . t)
      (lisp . t)
      (lua . t)
      (makefile . t)
      (js . t)
      (org . t)
      (plantuml . t)
      (python . t)
      (scheme . t)
      (sql . t)
      (sqlite . t)
      (emacs-lisp . t)))))

(message "Org mode loaded")

(use-package org-inlinetask
  :after org)

(use-package org-roam
  :ensure t
  :after org
  :init
  :custom
  (org-roam-directory antler/org-roam-directory)
  (org-roam-complete-everywhere t)
  :config
  (org-roam-db-autosync-mode))

(provide 'org-mode)
