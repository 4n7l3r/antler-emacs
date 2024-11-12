;;; lang-elisp.el --- Emacs Lisp configuration -*- lexical-binding: t -*-
;; First, ensure the package is installed
(use-package rainbow-delimiters
  :ensure t
  :defer t  ; Defer loading until needed
  :hook
  ((prog-mode . rainbow-delimiters-mode)
   (emacs-lisp-mode . rainbow-delimiters-mode)))

(use-package elisp-mode
  :after rainbow-delimiters
  :hook
  (emacs-lisp-mode . (lambda ()
                       (setq mode-name "ELisp")
                       (eldoc-mode)
                       (rainbow-delimiters-mode)
                       (prettify-symbols-mode)))
  :bind
  (:map emacs-lisp-mode-map
        ("C-c C-b" . eval-buffer)
        ("C-c C-c" . eval-defun)
        ("C-c C-r" . eval-region)))

(use-package eldoc
  :custom
  (eldoc-echo-area-use-multiline-p t)
  (eldoc-echo-area-prefer-doc-buffer t)
  :config
  (global-eldoc-mode))

(use-package macrostep
  :ensure t
  :bind
  (:map emacs-lisp-mode-map
        ("C-c e" . macrostep-expand)))

(use-package eros
  :ensure t
  :hook
  (emacs-lisp-mode . eros-mode))

(use-package elisp-slime-nav
  :ensure t
  :hook
  ((emacs-lisp-mode ielm-mode) . elisp-slime-nav-mode))


(defun sanityinc/headerise-elisp ()
  "Add minimal header and footer to an elisp buffer in order to placate flycheck."
  (interactive)
  (let ((fname (if (buffer-file-name)
                   (file-name-nondirectory (buffer-file-name))
                 (error "This buffer is not visiting a file"))))
    (save-excursion
      (goto-char (point-min))
      (insert ";;; " fname " --- Insert description here -*- lexical-binding: t -*-\n"
              ";;; Commentary:\n"
              ";;; Code:\n\n")
      (goto-char (point-max))
      (insert ";;; " fname " ends here\n"))))

;; Make C-x C-e run 'eval-region if the region is active
(defun sanityinc/eval-last-sexp-or-region (prefix)
  "Eval region from BEG to END if active, otherwise the last sexp."
  (interactive "P")
  (if (and (mark) (use-region-p))
      (eval-region (min (point) (mark)) (max (point) (mark)))
    (pp-eval-last-sexp prefix)))

(global-set-key [remap eval-expression] 'pp-eval-expression)

;; TODO: Refactor into normal config
(with-eval-after-load 'lisp-mode
  (define-key emacs-lisp-mode-map (kbd "C-x C-e") 'sanityinc/eval-last-sexp-or-region)
  (define-key emacs-lisp-mode-map (kbd "C-c C-e") 'pp-eval-expression))

(use-package ipretty
  :ensure t
  :init
  (add-hook 'after-init-hook 'ipretty-mode))


(defun sanityinc/make-read-only (_expression out-buffer-name &rest _)
  "Enable `view-mode' in the output buffer - if any - so it can be closed with `\"q\"."
  (when (get-buffer out-buffer-name)
    (with-current-buffer out-buffer-name
      (view-mode 1))))
(advice-add 'pp-display-expression :after 'sanityinc/make-read-only)

(defun sanityinc/load-this-file ()
  "Load the current file or buffer.
The current directory is temporarily added to `load-path'.  When
there is no current file, eval the current buffer."
  (interactive)
  (let ((load-path (cons default-directory load-path))
        (file (buffer-file-name)))
    (if file
        (progn
          (save-some-buffers nil (apply-partially 'derived-mode-p 'emacs-lisp-mode))
          (load-file (buffer-file-name))
          (message "Loaded %s" file))
      (eval-buffer)
      (message "Evaluated %s" (current-buffer)))))

(with-eval-after-load 'lisp-mode
  (define-key emacs-lisp-mode-map (kbd "C-c C-l") 'sanityinc/load-this-file))

(defun sanityinc/maybe-set-bundled-elisp-readonly ()
  "If this elisp appears to be part of Emacs, then disallow editing."
  (when (and (buffer-file-name)
             (string-match-p "\\.el\\.gz\\'" (buffer-file-name)))
    (setq buffer-read-only t)
    (view-mode 1)))

(add-hook 'emacs-lisp-mode-hook 'sanityinc/maybe-set-bundled-elisp-readonly)

;; Hippie-expand

(defun set-up-hippie-expand-for-elisp ()
  "Locally set `hippie-expand' completion functions for use with Emacs Lisp."
  (make-local-variable 'hippie-expand-try-functions-list)
  (add-to-list 'hippie-expand-try-functions-list 'try-complete-lisp-symbol t)
  (add-to-list 'hippie-expand-try-functions-list 'try-complete-lisp-symbol-partially t))

;; Automatic byte compilation
(use-package auto-compile
  :ensure t
  :init
  (setq auto-compile-delete-stray-dest nil)
  (add-hook 'after-init-hook 'auto-compile-on-save-mode)
  (add-hook 'after-init-hook 'auto-compile-on-load-mode)
  ;; Load .el if newer than corresponding .elc
  (setq load-prefer-newer t))

(use-package immortal-scratch
  :ensure t
  :init
  (add-hook 'after-init-hook 'immortal-scratch-mode))

;;; Support byte-compilation in a sub-process, as
;;; required by highlight-cl

(defun sanityinc/byte-compile-file-batch (filename)
  "Byte-compile FILENAME in batch mode, ie. a clean sub-process."
  (interactive "fFile to byte-compile in batch mode: ")
  (let ((emacs (car command-line-args)))
    (compile
     (concat
      emacs " "
      (mapconcat
       'shell-quote-argument
       (list "-Q" "-batch" "-f" "batch-byte-compile" filename)
       " ")))))

;; Enable desired features for all lisp modes
(defun sanityinc/enable-check-parens-on-save ()
  "Run `check-parens' when the current buffer is saved."
  (add-hook 'after-save-hook #'check-parens nil t))

(defvar sanityinc/lispy-modes-hook
  '(enable-paredit-mode
    sanityinc/enable-check-parens-on-save)
  "Hook run in all Lisp modes.")

(add-to-list 'sanityinc/lispy-modes-hook 'aggressive-indent-mode)

(defun sanityinc/lisp-setup ()
  "Enable features useful in any Lisp mode."
  (run-hooks 'sanityinc/lispy-modes-hook))

(use-package helpful
  :ensure t
  :bind
  (("C-h f" . helpful-callable)
   ("C-h v" . helpful-variable)
   ("C-h k" . helpful-key)
   ("C-h x" . helpful-command)
   ("C-c C-d" . helpful-at-point)))

(provide 'lang-elisp)
