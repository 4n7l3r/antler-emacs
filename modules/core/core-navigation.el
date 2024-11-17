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

(use-package ranger
  :ensure t
  :defer t
  :custom
  ;; Be evil if evil-mode is enabled
  (ranger-override-dired 'ranger)      ; Use ranger instead of dired
  (ranger-cleanup-on-disable t)        ; Kill buffers when closing ranger
  (ranger-cleanup-eagerly t)           ; Kill buffers as you move
  (ranger-show-hidden t)               ; Show hidden files
  (ranger-show-preview t)              ; Show previews
  (ranger-preview-file t)              ; Preview files
  (ranger-width-preview 0.5)           ; Preview window width
  (ranger-max-preview-size 10)         ; Don't preview files > 10MB
  (ranger-dont-show-binary t)          ; Don't show binary files in preview

  ;; Parent window settings
  (ranger-parent-depth 2)              ; Number of parent directories to show
  (ranger-width-parents 0.12)          ; Width of parent windows
  (ranger-max-parent-width 0.12)       ; Max width of parent windows

  ;; Header customization
  (ranger-header-func 'ranger-header-line) ; Default header style
  (ranger-modify-header t)                 ; Modify header

  ;; Sorting and filtering
  (ranger-sorting-switches "-alh")     ; Default sorting
  (ranger-persistent-sort t)           ; Remember sorting

  ;; Preview customization
  (ranger-excluded-extensions '("mkv" "iso" "mp4")) ; Don't preview these
  (ranger-preview-delay 0.040)         ; Delay before showing preview

  :config
  ;; Additional hooks
  (add-hook 'ranger-mode-hook
            (lambda ()
              (setq-local line-spacing 3)      ; Add some line spacing
              (setq-local cursor-type 'bar)    ; Use bar cursor
              (hl-line-mode 1)))               ; Highlight current line

  ;; Custom functions
  (defun antler/ranger-search-files ()
    "Search files in current directory using consult."
    (interactive)
    (consult-find (ranger-current-directory)))

  :bind
  (:map global-map
        ("C-x d" . ranger)
        ("C-x C-d" . deer)))

(provide 'core-navigation)
