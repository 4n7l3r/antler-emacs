;;; completion-vertico.el --- Vertico completion setup -*- lexical-binding: t -*-

(use-package vertico
  :ensure t
  :custom
  (vertico-cycle t)
  (vertico-resize t)
  (vertico-count 17)
  :bind (:map vertico-map
              ("<tab>" . vertico-insert)
              ("<escape>" . minibuffer-keyboard-quit)
              ("M-." . vertico-repeat))
  :hook (minibuffer-setup . vertico-repeat-save)
  :init
  (vertico-mode))

(use-package marginalia
  :ensure t
  :after vertico
  :custom
  (marginalia-max-relative-age 0)
  (marginalia-align 'right)
  :bind (:map minibuffer-local-map
              ("M-A" . marginalia-cycle))
  :init
  (marginalia-mode))

(use-package orderless
  :ensure t
  :custom
  (completion-styles '(orderless basic))
  (completion-category-overrides
   '((file (styles . (basic partial-completion orderless))))))

(use-package embark
  :ensure t
  :after vertico
  :bind
  (:map vertico-map
        ("C-c C-o" . embark-export)
        ("C-c C-c" . embark-act)))

(provide 'completion-vertico)
