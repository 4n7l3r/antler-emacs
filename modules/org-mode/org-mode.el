;;; org-mode.el --- Org mode configuration -*- lexical-binding: t -*-

(defcustom antler/org-directory (concat (getenv "HOME") "/org")
  "Base directory for org files."
  :type 'directory
  :group 'antler)

(defcustom antler/org-roam-directory (concat (getenv "HOME") "/org")
  "Base directory for org files."
  :type 'directory
  :group 'antler)

(use-package org
  :hook ((org-mode . org-indent-mode)
	 (org-mode . visual-line-mode)
	 (org-mode . variable-pitch-mode))
  :config
  (setq org-directory antler/org-directory)
  (setq org-default-notes-file (concat org-directory "/notes.org"))
  (setq org-agenda-files (concat org-directory "/tasks.org"))
  (setq org-return-follows-link t)
  (add-to-list 'auto-mode-alist '("\\.org\\'" . org-mode))
  (add-hook 'org-mode-hook 'org-indent-mode)
  (org-babel-do-load-languages
   'org-babel-load-languages
   (seq-filter
    (lambda (pair)
      (locate-library (concat "ob-" (symbol-name (car pair)))))
    '((R . t)
      (ditaa . t)
      (dot . t)
      (emacs-lisp . t)
      (gnuplot . t)
      (haskell . nil)
      (latex . t)
      (ledger . t)
      (ocaml . nil)
      (octave . t)
      (plantuml . t)
      (python . t)
      (ruby . t)
      (screen . nil)
      (sh . t) ;; obsolete
      (shell . t)
      (sql . t)
      (sqlite . t)))))

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
