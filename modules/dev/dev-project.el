;;; dev-project.el --- Project management -*- lexical-binding: t -*-

;; Utilize Emacs 29+ built-in project.el with enhancements
(use-package project
  :bind-keymap ("C-x p" . project-prefix-map)
  :bind (:map project-prefix-map
         ("f" . project-find-file)
         ("b" . project-switch-to-buffer)
         ("k" . project-kill-buffers)
         ("p" . project-switch-project)
         ("d" . project-find-dir)
         ("g" . project-find-regexp)
         ("!" . project-shell-command)
         ("&" . project-async-shell-command)
         ("e" . project-eshell))
  :custom
  (project-switch-commands 'project-find-file)
  :config
  ;; Add more project roots
  (setq project-vc-extra-root-markers '("package.json" "Cargo.toml" "go.mod" "requirements.txt" "pyproject.toml"))
  
  ;; Basic project enhancements that don't require project-x
  (defun project-remember-projects (project-root)
    "Remember the current PROJECT-ROOT for future use."
    (with-temp-buffer
      (insert project-root)
      (newline)
      (append-to-file (point-min) (point-max) 
                      (expand-file-name "projects" user-emacs-directory))))
  
  (advice-add 'project-remember-project :after #'project-remember-projects))

;; Enhanced project finding/switching
(use-package consult-project-extra
  :straight t
  :after (consult project)
  :bind (("C-c p f" . consult-project-extra-find)
         ("C-c p o" . consult-project-extra-find-other-window)))

;; Enhanced project file navigation
(use-package projectile
  :straight t
  :after project
  :diminish projectile-mode
  :bind-keymap ("C-c p" . projectile-command-map)
  :custom
  (projectile-project-search-path '("~/Projects/" "~/Work/"))
  (projectile-sort-order 'recently-active)
  (projectile-enable-caching t)
  (projectile-completion-system 'default)
  :config
  (projectile-mode +1))

;; Direnv integration for project environments
(use-package direnv
  :straight t
  :disabled t  ;; Disabled until direnv is installed
  :config
  (direnv-mode))

;; Alternative: Add instructions to install direnv
;; To enable, install direnv with your package manager:
;; macOS: brew install direnv
;; Linux: apt install direnv or equivalent
;; Then remove the :disabled t line above

(provide 'dev-project) 