;;; completion-vertico.el --- Vertico completion setup -*- lexical-binding: t -*-

(use-package vertico
  :straight t
  :custom
  (vertico-cycle t)
  (vertico-resize t)
  (vertico-count 17)
  (vertico-scroll-margin 5)
  (vertico-multiline
   (cons #("⤶" 0 1 (face vertico-multiline))
         #("…" 0 1 (face vertico-multiline))))
  (vertico-mouse-mode t)
  :bind (:map vertico-map
              ("<tab>" . vertico-insert)
              ("<escape>" . minibuffer-keyboard-quit)
              ("M-." . vertico-repeat)
              ("DEL" . vertico-directory-delete-char)
              ("M-DEL" . vertico-directory-delete-word)
              ("C-w" . vertico-directory-delete-word)
              ("RET" . vertico-directory-enter))
  :hook (minibuffer-setup . vertico-repeat-save)
  :init
  (vertico-mode)
  :config
  (require 'vertico-directory))

(use-package marginalia
  :straight t
  :after vertico
  :custom
  (marginalia-max-relative-age 0)
  (marginalia-align 'right)
  (marginalia-separator "  ")
  :bind (:map minibuffer-local-map
              ("M-A" . marginalia-cycle))
  :init
  (marginalia-mode))

(use-package orderless
  :straight t
  :custom
  (completion-styles '(orderless basic))
  (completion-category-overrides
   '((file (styles . (basic partial-completion orderless)))
     (command (styles orderless))
     (variable (styles orderless))
     (symbol (styles orderless))
     (consult-location (styles orderless))))
  (orderless-component-separator #'orderless-escapable-split-on-space)
  (orderless-matching-styles '(orderless-literal
                              orderless-prefixes
                              orderless-initialism
                              orderless-regexp)))

(use-package embark
  :straight t
  :after vertico
  :bind
  (("C-." . embark-act)
   ("C-;" . embark-dwim)
   ("C-h B" . embark-bindings)
   :map minibuffer-local-map
   ("C-c C-o" . embark-export)
   ("C-c C-c" . embark-act))
  :custom
  (embark-indicators
   '(embark-minimal-indicator
     embark-highlight-indicator
     embark-isearch-highlight-indicator))
  (embark-recall-indicator nil)
  (prefix-help-command #'embark-prefix-help-command)
  :config
  (add-to-list 'display-buffer-alist
               '("\\`\\*Embark Collect \\(Live\\|Completions\\)\\*"
                 nil
                 (window-parameters (mode-line-format . none)))))

(use-package embark-consult
  :straight t
  :after (embark consult)
  :hook
  (embark-collect-mode . consult-preview-at-point-mode))

(provide 'completion-vertico)
