;;; completion-corfu.el --- Corfu completion configuration -*- lexical-binding: t -*-

;; Use child frames on graphical displays, regular popup on terminals
(defvar antler/corfu-use-child-frame (display-graphic-p))

(use-package corfu
  :straight t
  :custom
  (corfu-auto t)                       ;; Enable auto completion
  (corfu-auto-prefix 2)                ;; Complete with at least 2 characters
  (corfu-auto-delay 0.2)               ;; Delay for auto completion
  (corfu-min-width 25)
  (corfu-max-width 80)
  (corfu-count 15)                     ;; Show more candidates
  (corfu-scroll-margin 5)
  (corfu-cycle t)                      ;; Enable cycling
  (corfu-quit-at-boundary t)           ;; Quit at completion boundary
  (corfu-quit-no-match t)              ;; Quit if there's no match 
  (corfu-preview-current 'insert)      ;; Preview current candidate
  (corfu-preselect 'prompt)            ;; Preselect the prompt
  (corfu-on-exact-match nil)           ;; Don't auto-confirm exact matches
  (corfu-echo-documentation t)         ;; Show documentation in echo area
  (corfu-popupinfo-delay 0.2)          ;; Delay before showing documentation popup
  :bind
  (:map corfu-map
        ("TAB" . corfu-next)
        ([tab] . corfu-next)
        ("S-TAB" . corfu-previous)
        ([backtab] . corfu-previous)
        ("M-d" . corfu-info-documentation)
        ("M-l" . corfu-info-location)
        ("M-SPC" . corfu-insert-separator))
  :init
  (global-corfu-mode)
  :config
  ;; Enable Corfu completion for minibuffer prompts
  (defun corfu-enable-in-minibuffer ()
    "Enable Corfu in the minibuffer if Vertico is not active."
    (unless (bound-and-true-p vertico--input)
      (corfu-mode 1)))
  (add-hook 'minibuffer-setup-hook #'corfu-enable-in-minibuffer))

;; Child frame support for Corfu completions
(use-package corfu-popupinfo
  :straight nil  ;; Part of corfu
  :after corfu
  :hook (corfu-mode . corfu-popupinfo-mode)
  :custom
  (corfu-popupinfo-delay 0.2)
  (corfu-popupinfo-max-width 80)
  (corfu-popupinfo-max-height 15))

;; Enhanced child-frame UI for Corfu
;; Note: Using corfu-candidate-overlay requires corfu-auto to be disabled
(when antler/corfu-use-child-frame
  (use-package corfu-candidate-overlay
    :straight t
    :after corfu
    :disabled t  ;; Disabled due to conflict with corfu-auto
    :custom
    (corfu-candidate-overlay-auto-update-delay 0.2)
    :config
    ;; This is commented out because it conflicts with corfu-auto
    ;; To use this, you must set corfu-auto to nil
    ;; (corfu-candidate-overlay-mode +1)
    ))

;; Enhance documentation rendering
(use-package cape
  :straight t
  :init
  ;; Add emacs-lisp-mode to lsp-bridge-default-mode if needed
  :bind (("C-c p p" . completion-at-point)  ;; Capf
         ("C-c p t" . complete-tag)         ;; etags
         ("C-c p d" . cape-dabbrev)         ;; or dabbrev-completion
         ("C-c p f" . cape-file)
         ("C-c p k" . cape-keyword)
         ("C-c p s" . cape-symbol)
         ("C-c p a" . cape-abbrev)
         ("C-c p i" . cape-ispell)
         ("C-c p l" . cape-line)
         ("C-c p w" . cape-dict))
  :config
  ;; Add completion backends
  (add-to-list 'completion-at-point-functions #'cape-file)
  (add-to-list 'completion-at-point-functions #'cape-dabbrev)
  (add-to-list 'completion-at-point-functions #'cape-keyword)
  
  ;; Silence then pcomplete capf, no errors or messages
  (advice-add 'pcomplete-completions-at-point :around #'cape-wrap-silent)
  
  ;; Ensure case-sensitivity for file completion
  (add-to-list 'completion-category-defaults
               '(file (styles orderless))))

;; Enhanced snippet expansion with Tempel 
(use-package tempel
  :straight t
  :custom
  (tempel-trigger-prefix "<")
  :bind (("M-+" . tempel-complete) ;; Alternative tempel-expand
         ("M-*" . tempel-insert))
  :config
  (defun tempel-setup-capf ()
    ;; Add the Tempel Capf to `completion-at-point-functions'.
    ;; `tempel-expand' only triggers on exact matches.
    (setq-local completion-at-point-functions
                (cons #'tempel-complete
                      completion-at-point-functions)))
  
  ;; Hooks for programming and text modes
  (add-hook 'prog-mode-hook 'tempel-setup-capf)
  (add-hook 'text-mode-hook 'tempel-setup-capf))

(provide 'completion-corfu)
