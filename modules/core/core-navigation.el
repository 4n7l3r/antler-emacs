;;; core-navigation.el --- Navigation enhancement tools -*- lexical-binding: t -*-

(use-package ace-window
  :ensure t
  :bind (("M-o" . ace-window))
  :custom
  (aw-keys '(?a ?s ?d ?f ?g ?h ?j ?k ?l))
  (aw-scope 'frame)
  (aw-background t)
  (setq aw-dispatch-alist((?x aw-delete-window "Delete Window")
			  (?m aw-swap-window "Swap Windows")
			  (?M aw-move-window "Move Window")
			  (?c aw-copy-window "Copy Window")
			  (?j aw-switch-buffer-in-window "Select Buffer")
			  (?n aw-flip-window)
			  (?u aw-switch-buffer-other-window "Switch Buffer Other Window")
			  (?c aw-split-window-fair "Split Fair Window")
			  (?v aw-split-window-vert "Split Vert Window")
			  (?b aw-split-window-horz "Split Horz Window")
			  (?o delete-other-windows "Delete Other Windows")
			  (?? aw-show-dispatch-help)))
  (aw-dispatch-always t))

(use-package which-key
  :ensure t
  :defer 1
  :custom
  (which-key-idle-delay 0.5)
  (which-key-add-column-padding 1)
  (which-key-max-display-columns nil)
  :config
  (which-key-mode))

;; Prefer g-prefixed coreutils version of standard utilities when available
(let ((gls (executable-find "gls")))
  (when gls (setq insert-directory-program gls)))

(use-package diredfl
  :ensure t
  :after dired
  :init
  (diredfl-global-mode)
  (require 'dired-x))

;; Hook up dired-x global bindings without loading it up-front
(define-key ctl-x-map "\C-j" 'dired-jump)
(define-key ctl-x-4-map "\C-j" 'dired-jump-other-window)


(use-package diff-hl
  :ensure t
  :after dired
  :init
  (add-hook 'dired-mode-hook 'diff-hl-dired-mode))

(use-package dirvish
  :ensure t
  :init
  (dirvish-override-dired-mode)
  :custom
  (dirvish-quick-access-entries
   '(("e" "~/.config/" "Emacs Config")
     ("o" "~/org" "Org")))
  :config
  (dirvish-override-dired-mode)
  (dirvish-peek-mode)
  (dirvish-side-follow-mode)
  (setq dirvish-attributes
      '(vc-state file-size git-msg subtree-state all-the-icons collapse file-time))
  (setq dirvish-mode-line-format '(:left (sort symlink) :right (vc-info yank index)))
  (setq dirvish-header-line-height '(25 . 35))
  (setq dirvish-header-line-format '(:left (path) :right (free-space)))
  (setq dired-listing-switches
      "-l --almost-all --human-readable --group-directories-first --no-group"))
(provide 'core-navigation)
