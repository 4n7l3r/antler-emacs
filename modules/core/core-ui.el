;;; core-ui.el --- UI Configuration -*- lexical-binding: t -*-

;; Basic UI settings
(setq inhibit-startup-message t)
(setq visible-bell t)
(scroll-bar-mode -1)
(tool-bar-mode -1)
(tooltip-mode -1)
(menu-bar-mode -1)
(set-fringe-mode 10)

;;; Integrate with terminals such as xterm
(use-package mwheel
  :bind
  ([mouse-4] . (lambda () (interactive) (scroll-down 1)))
  ([mouse-5] . (lambda () (interactive) (scroll-up 1)))
  :init
  (autoload 'mwheel-install "mwheel"))

(defun sanityinc/console-frame-setup ()
  (xterm-mouse-mode 1) ; Mouse in a terminal (Use shift to paste with middle button)
  (mwheel-install))

(add-hook 'after-make-console-frame-hooks 'sanityinc/console-frame-setup)

;; Stop C-z from minimizing windows under OS X
(defun sanityinc/maybe-suspend-frame ()
  (interactive)
  (unless (and *is-a-mac* window-system)
    (suspend-frame)))

(bind-key "C-z" 'sanityinc/maybe-suspend-frame)

;; Font configuration
(set-face-attribute 'default nil :font "FiraCode Nerd Font" :height 120)
(set-face-attribute 'fixed-pitch nil :font "FiraCode Nerd Font Mono" :height 120)
(set-face-attribute 'variable-pitch nil :font "FiraCode Nerd Font" :height 120 :weight 'regular)

;; Window size and features
(setq-default window-resize-pixelwise t
              frame-resize-pixelwise t)

(let ((no-border '(internal-border-width . 0)))
  (add-to-list 'default-frame-alist no-border)
  (add-to-list 'initial-frame-alist no-border))

(defun sanityinc/adjust-opacity (frame incr)
  "Adjust the background opacity of FRAME by increment INCR."
  (unless (display-graphic-p frame)
    (error "Cannot adjust opacity of this frame"))
  (let* ((oldalpha (or (frame-parameter frame 'alpha) 100))
         ;; The 'alpha frame param became a pair at some point in
         ;; emacs 24.x, e.g. (100 100)
         (oldalpha (if (listp oldalpha) (car oldalpha) oldalpha))
         (newalpha (+ incr oldalpha)))
    (when (and (<= frame-alpha-lower-limit newalpha) (>= 100 newalpha))
      (modify-frame-parameters frame (list (cons 'alpha newalpha))))))

(use-package frame
  :bind
  (("M-C-8" . (lambda () (interactive) (sanityinc/adjust-opacity nil -2)))
   ("M-C-9" . (lambda () (interactive) (sanityinc/adjust-opacity nil 2)))
   ("M-C-7" . (lambda () (interactive) (modify-frame-parameters nil `((alpha . 100)))))
   ("M-ƒ" . toggle-frame-fullscreen)))

(setq frame-title-format
      '((:eval (if (buffer-file-name)
                   (abbreviate-file-name (buffer-file-name))
                 "%b"))))

;; Non-zero values for `line-spacing' can mess up ansi-term and co,
;; so we zero it explicitly in those cases.
(add-hook 'term-mode-hook
          (lambda ()
            (setq line-spacing 0)))

(use-package default-text-scale
  :ensure t
  :init
  (add-hook 'after-init-hook 'default-text-scale-mode))

;; Suppress GUI features
(setq use-file-dialog nil)
(setq use-dialog-box nil)
(setq inhibit-startup-screen t)

;; Frame transparency
(set-frame-parameter nil 'alpha-background 90)
(add-to-list 'default-frame-alist '(alpha-background . 90))

;; Line numbers
(column-number-mode)
(global-display-line-numbers-mode t)
(dolist (mode '(org-mode-hook
                term-mode-hook
                eshell-mode-hook
                shell-mode-hook))
  (add-hook mode (lambda () (display-line-numbers-mode 0))))

(setq inhibit-startup-message t)
(setq-default initial-scratch-message (concat ";; The rabbit's just a monkey in disguise 👀\n\n"))

(defvar after-make-console-frame-hooks '()
  "Hooks to run after creating a new TTY frame")
(defvar after-make-window-system-frame-hooks '()
  "Hooks to run after creating a new window-system frame")

(defun run-after-make-frame-hooks (frame)
  "Run configured hooks in response to the newly-created FRAME.
Selectively runs either `after-make-console-frame-hooks' or
`after-make-window-system-frame-hooks'"
  (with-selected-frame frame
    (run-hooks (if window-system
                   'after-make-window-system-frame-hooks
                 'after-make-console-frame-hooks))))

(add-hook 'after-make-frame-functions 'run-after-make-frame-hooks)

(defconst sanityinc/initial-frame (selected-frame)
  "The frame (if any) active during Emacs initialization.")

(add-hook 'after-init-hook
          (lambda () (when sanityinc/initial-frame
                  (run-after-make-frame-hooks sanityinc/initial-frame))))

(provide 'core-ui)
