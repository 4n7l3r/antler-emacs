;;; org-mode.el --- Org mode configuration -*- lexical-binding: t -*-

(defcustom antler/org-directory (concat (getenv "HOME") "/org")
  "Base directory for org files."
  :type 'directory
  :group 'antler)

(defcustom antler/org-roam-directory (concat antler/org-directory "/roam")
  "Base directory for org roam files."
  :type 'directory
  :group 'antler)

(defcustom antler/org-journal-directory (concat antler/org-directory "/journal")
  "Base directory for org journal files."
  :type 'directory
  :group 'antler)

(defun antler/org-remap-move-keys ()
  "Disable M-<arrow> keybindings in Org mode for list manipulation."
  (let ((keys '("<M-up>" "<M-down>" "<M-left>" "<M-right>")))
    (dolist (key keys)
      (define-key org-mode-map (kbd key) nil))))

(require 'org-hypothesis)

;; Set up Org Mode fonts
(defun antler/org-font-setup ()
  "Set up fonts for org-mode."
  ;; Set faces for heading levels
  (dolist (face '((org-level-1 . 1.2)
                  (org-level-2 . 1.1)
                  (org-level-3 . 1.05)
                  (org-level-4 . 1.0)
                  (org-level-5 . 0.9)
                  (org-level-6 . 0.9)
                  (org-level-7 . 0.9)
                  (org-level-8 . 0.9)))
    (set-face-attribute (car face) nil :font "JetBrainsMono Nerd Font" :weight 'semi-bold :height (cdr face)))

  ;; Make sure org-indent face is a monospace face to avoid alignment issues
  (set-face-attribute 'org-indent nil :inherit '(org-hide fixed-pitch))

  ;; Ensure certain org faces use fixed-pitch font
  (dolist (face '(org-block org-code org-verbatim org-table org-formula org-meta-line org-checkbox org-checkbox-statistics-todo org-checkbox-statistics-done))
    (set-face-attribute face nil :inherit 'fixed-pitch)))

(use-package org
  :hook ((org-mode . org-indent-mode)
         (org-mode . visual-line-mode)
         (org-mode . variable-pitch-mode)
         (org-mode . antler/org-remap-move-keys)
         (org-mode . org-hypothesis-mode)
         (org-mode . antler/org-font-setup)
         (org-mode . (lambda()
                       (auto-fill-mode 1)
                       (setq fill-column 120))))
  :bind
  (("C-c a" . org-agenda)
   ("C-c c" . org-capture)
   ("C-c l" . org-store-link))
  :custom
  (org-directory antler/org-directory)
  (org-default-notes-file (concat antler/org-directory "/notes.org"))
  (org-agenda-files (list
                     (concat antler/org-directory "/tasks")
                     (concat antler/org-directory "/roam")
                     antler/org-journal-directory
                     antler/org-directory))
  (org-log-done 'time)
  (org-return-follows-link t)
  (org-hide-emphasis-markers t)
  (org-ellipsis " ⤵")
  (org-agenda-start-with-log-mode t)
  (org-agenda-include-diary t)
  (org-pretty-entities t)
  (org-appear-autolinks t)
  (org-capture-templates
   '(("t" "Task" entry (file+headline "~/org/tasks.org" "Inbox")
      "* TODO %?\n  %U\n  %a\n  %i" :empty-lines 1)
     ("j" "Journal" entry (file+olp+datetree "~/org/journal.org")
      "* %U %?\n%i\n%a" :empty-lines 1)))
  :config
  (add-to-list 'auto-mode-alist '("\\.org\\'" . org-mode))

  ;; Improve org appearance
  (setq org-startup-indented t
        org-startup-with-inline-images t
        org-image-actual-width 600
        org-pretty-entities t
        org-hide-emphasis-markers t)

  ;; Babel languages setup
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

;; Inline tasks for quick TODO items
(with-eval-after-load 'org
  (require 'org-inlinetask nil t))

;; Auto toggle emphasis markers when cursor is on them
(use-package org-appear
  :straight t
  :hook (org-mode . org-appear-mode)
  :custom
  (org-appear-autoemphasis t)
  (org-appear-autolinks t)
  (org-appear-autosubmarkers t))

;; Modern UI for org mode
(use-package org-modern
  :straight t
  :hook (org-mode . org-modern-mode)
  :custom
  (org-modern-star ["◉" "○" "●" "○" "●" "○" "●"])
  (org-modern-hide-stars nil)
  (org-modern-table-vertical 1)
  (org-modern-table-horizontal 0.2)
  (org-modern-list '((43 . "➤")  ;; +
                    (45 . "–")  ;; -
                    (42 . "•"))) ;; *
  (org-modern-block-fringe t)
  (org-modern-tag t)
  (org-modern-priority t)
  (org-modern-keyword t)
  (org-modern-statistics t)
  :config
  (global-org-modern-mode))

;; Fancy TODO keywords
(use-package org-fancy-priorities
  :straight t
  :hook (org-mode . org-fancy-priorities-mode)
  :custom
  (org-fancy-priorities-list '("⚑" "⧖" "⬇" "☕")))

;; Org Roam for networked note-taking
(use-package org-roam
  :straight t
  :after org
  :bind (("C-c n l" . org-roam-buffer-toggle)
         ("C-c n f" . org-roam-node-find)
         ("C-c n g" . org-roam-graph)
         ("C-c n i" . org-roam-node-insert)
         ("C-c n c" . org-roam-capture)
         ("C-c n j" . org-roam-dailies-capture-today)
         ("C-c n t" . org-roam-dailies-goto-today)
         ("C-c n y" . org-roam-dailies-goto-yesterday)
         ("C-c n r" . org-roam-dailies-goto-tomorrow))
  :custom
  (org-roam-directory antler/org-roam-directory)
  (org-roam-complete-everywhere t)
  (org-roam-capture-templates
   '(("d" "default" plain "%?"
      :target (file+head "${slug}.org"
                         "#+title: ${title}\n#+date: %U\n#+filetags: :note:\n")
      :unnarrowed t)
     ("p" "project" plain "%?"
      :target (file+head "${slug}.org"
                         "#+title: ${title}\n#+date: %U\n#+filetags: :project:\n")
      :unnarrowed t)
     ("r" "reference" plain "%?"
      :target (file+head "${slug}.org"
                         "#+title: ${title}\n#+date: %U\n#+filetags: :reference:\n")
      :unnarrowed t)))
  :config
  (org-roam-db-autosync-mode))

;; Org Journal for daily entries
(use-package org-journal
  :straight t
  :defer t
  :custom
  (org-journal-dir antler/org-journal-directory)
  (org-journal-date-format "%A, %Y-%m-%d")
  (org-journal-file-format "%Y-%m-%d.org")
  (org-journal-file-type 'monthly)
  :config
  (unless (file-exists-p org-journal-dir)
    (make-directory org-journal-dir t)))

;; Link handling setup
(setq org-link-frame-setup
      '((vm . vm-visit-folder-other-frame)
        (vm-imap . vm-visit-imap-folder-other-frame)
        (gnus . org-gnus-no-new-news)
        (file . find-file)
        (wl . wl-other-frame)))

(provide 'org-mode)
