;;; completion-corfu.el --- Corfu completion setup -*- lexical-binding: t -*-

(use-package corfu
  :ensure t
  :custom
  (corfu-cycle t)
  (corfu-auto t)
  (corfu-auto-prefix 2)
  (corfu-auto-delay 0.25)
  (corfu-min-width 80)
  (corfu-max-width 80)
  (corfu-count 14)
  (corfu-preview-current nil)
  (corfu-preselect-first nil)
  (corfu-quit-at-boundary t)
  (corfu-quit-no-match t)
  (corfu-echo-documentation t)
  :hook
  ((prog-mode . corfu-mode)
   (shell-mode . corfu-mode)
   (eshell-mode . corfu-mode))
  :bind
  (:map corfu-map
        ;;("M-i" . completion-at-point)    ; Manual trigger
        ("ESC" . corfu-quit)
        ;;("M-n" . corfu-next)
        ;;("M-p" . corfu-previous)
        )
  :init
  (global-corfu-mode))

(use-package corfu-popupinfo
  :after corfu
  :custom
  (corfu-popupinfo-delay 0)
  (corfu-popupinfo-hide nil)
  :init
  (corfu-popupinfo-mode)
  :config

  (defun antler/corfu-enable-always-display-documentation ()
    "Show the full documentation without truncation."
    (let ((doc (corfu-popupinfo--get-documentation)))
      (when doc
        (with-temp-buffer
          (insert doc)
          (corfu-popupinfo--display-string (buffer-string))))))

  (setq corfu-popupinfo-display-function
        #'antler/corfu-enable-always-display-documentation)

  ;; Documentation navigation
  (define-key corfu-map (kbd "M-d") #'corfu-popupinfo-toggle)
  (define-key corfu-map (kbd "M-p") #'corfu-popupinfo-scroll-down)
  (define-key corfu-map (kbd "M-n") #'corfu-popupinfo-scroll-up))

(use-package kind-icon
  :ensure t
  :after corfu
  :custom
  (kind-icon-default-face 'corfu-default)
  :config
  (add-to-list 'corfu-margin-formatters #'kind-icon-margin-formatter))

(provide 'completion-corfu)
