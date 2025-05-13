;;; completion-consult.el --- Consult setup -*- lexical-binding: t -*-

(use-package consult
  :straight t
  :bind (;; C-c bindings in `mode-specific-map'
         ("C-c M-x" . consult-mode-command)
         ("C-c h" . consult-history)
         ("C-c k" . consult-kmacro)
         ("C-c m" . consult-man)
         ("C-c i" . consult-info)
         ("C-c r" . consult-ripgrep)
         ;; C-x bindings in `ctl-x-map'
         ("C-x M-:" . consult-complex-command)     ;; orig. repeat-complex-command
         ("C-x b" . consult-buffer)                ;; orig. switch-to-buffer
         ("C-x 4 b" . consult-buffer-other-window) ;; orig. switch-to-buffer-other-window
         ("C-x 5 b" . consult-buffer-other-frame)  ;; orig. switch-to-buffer-other-frame
         ("C-x t b" . consult-buffer-other-tab)    ;; For Emacs 31, tab-bar support
         ("C-x r b" . consult-bookmark)            ;; orig. bookmark-jump
         ("C-x p b" . consult-project-buffer)      ;; from project.el
         ;; Custom M-# bindings for fast register access
         ("M-#" . consult-register-load)
         ("M-'" . consult-register-store)          ;; use register more often
         ;; Other custom bindings
         ("M-y" . consult-yank-pop)                ;; reimp. `yank-pop'
         ;; M-g bindings in `goto-map'
         ("M-g e" . consult-compile-error)
         ("M-g f" . consult-flymake)               ;; alt. consult-flycheck
         ("M-g g" . consult-goto-line)             ;; orig. goto-line
         ("M-g M-g" . consult-goto-line)           ;; orig. goto-line
         ("M-g o" . consult-outline)               ;; navigate headings
         ("M-g m" . consult-mark)                  ;; for mark navigation
         ("M-g k" . consult-global-mark)           ;; for global mark navigation
         ("M-g i" . consult-imenu)                 ;; better than imenu
         ("M-g I" . consult-imenu-multi)           ;; imenu in multiple buffers
         ;; M-s bindings in `search-map'
         ("M-s d" . consult-find)                  ;; use built-in consult-find (instead of fd)
         ("M-s D" . consult-locate)                ;; use locate for file search
         ("M-s g" . consult-grep)                  ;; search with grep
         ("M-s G" . consult-git-grep)              ;; search with git-grep
         ("M-s r" . consult-ripgrep)               ;; powerful search with ripgrep
         ("M-s l" . consult-line)                  ;; search current buffer
         ("M-s L" . consult-line-multi)            ;; search multiple buffers
         ("M-s k" . consult-keep-lines)            ;; keep lines matching expr
         ("M-s u" . consult-focus-lines)           ;; remove lines matching expr
         ;; Isearch integration
         ("M-s e" . consult-isearch-history)       ;; isearch history
         :map isearch-mode-map
         ("M-e" . consult-isearch-history)         ;; orig. isearch-edit-string
         ("M-s e" . consult-isearch-history)       ;; orig. isearch-edit-string
         ("M-s l" . consult-line)                  ;; search in current buffer
         ("M-s L" . consult-line-multi))           ;; search in multiple buffers
  
  :custom
  ;; For Emacs 31, integrate with project.el
  (xref-show-xrefs-function #'consult-xref)
  (xref-show-definitions-function #'consult-xref)
  
  ;; Configure the narrowing key.
  (consult-narrow-key "C-+")
  
  ;; Preview settings
  (consult-preview-key "M-.")
  (consult-preview-max-size 10485760) ;; 10MB for large files
  (consult-preview-max-count 10)       ;; Limit preview to 10 files
  
  ;; Configure preview for files and buffers
  (consult-preview-key
   (list :debounce 0.5 'any))
  
  ;; Use project.el for finding project root
  (consult-project-function #'consult--default-project-function)
  
  :init
  ;; Use Consult to select xref locations with preview
  (setq xref-show-xrefs-function #'consult-xref
        xref-show-definitions-function #'consult-xref)
  
  :config
  ;; Custom fd integration without requiring external package
  (when (executable-find "fd")
    (defun consult-fd (&optional dir initial)
      "Search for files with `fd' in DIR with INITIAL input."
      (interactive "P")
      (let* ((dir (expand-file-name
                   (if dir
                       (read-directory-name "Directory: ")
                     (if current-prefix-arg
                         (read-directory-name "Directory: ")
                       default-directory))))
             (prompt-dir
              (consult--directory-prompt "Find file" dir))
             (default-directory dir))
        (find-file
         (consult--read
          (consult--async-command
              "fd --type f --color=never --full-path"
            (consult--async-map (lambda (line) line)
                                 (consult--async-highlight
                                 (consult--regexp-filter initial))))
          :prompt prompt-dir
          :category 'file
          :sort nil
          :require-match t
          :initial initial
          :history 'file-name-history
          :state (consult--file-preview)))))
    
    ;; Add keybinding for fd search
    (global-set-key (kbd "M-s d") #'consult-fd)))

;; Enhanced directory navigation with consult-dir
(use-package consult-dir
  :straight t
  :after consult
  :bind (("C-x C-d" . consult-dir)
         :map minibuffer-local-map
         ("C-x C-d" . consult-dir)
         ("C-x C-j" . consult-dir-jump-file))
  :custom
  (consult-dir-project-list-function #'consult-dir-project-dirs))

;; Enhanced project navigation
(use-package consult-project-extra
  :straight t
  :after (consult project)
  :bind
  (("C-c p f" . consult-project-extra-find)
   ("C-c p o" . consult-project-extra-find-other-window)))

(provide 'completion-consult)
