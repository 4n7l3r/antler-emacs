;;; ui-theme.el --- Theme configuration -*- lexical-binding: t -*-

(use-package doom-themes
  :straight t
  :demand t  ;; Force immediate loading, don't defer
  :custom
  (doom-themes-enable-bold t)
  (doom-themes-enable-italic t)
  (doom-themes-padded-modeline nil) ; Don't add padding to modeline
  :init
  ;; Define hook for theme changes if not already defined
  (unless (boundp 'after-load-theme-hook)
    (defvar after-load-theme-hook nil
      "Hook run after a color theme is loaded using `load-theme'.")
    
    ;; Use advice-add instead of defadvice (no longer obsolete)
    (advice-add 'load-theme :after
                (lambda (&rest _args)
                  "Run `after-load-theme-hook'."
                  (run-hooks 'after-load-theme-hook))))
  
  ;; Resolve potential conflict with custom-safe-themes
  (setq custom-safe-themes t)  ;; Trust all themes
  
  :config
  ;; Load the theme explicitly
  (message "Loading doom theme...")
  (condition-case err
      (load-theme 'doom-1337 t)
    (error
     (message "Error loading theme: %s" (error-message-string err))
     ;; Try a fallback theme
     (condition-case nil
         (load-theme 'doom-one t)
       (error nil))))

  ;; Enable flashing mode-line on errors
  (doom-themes-visual-bell-config)

  ;; Corrects and improves org-mode's native fontification
  (doom-themes-org-config)
  
  ;; Ensure theme is applied to all frames
  (dolist (frame (frame-list))
    (with-selected-frame frame
      (enable-theme 'doom-1337))))

;; Force theme reapplication after GUI frames are created
(defun my/reapply-theme (_frame)
  "Reapply the current theme when a new frame is created."
  (let ((theme (car custom-enabled-themes)))
    (when theme
      (message "Reapplying theme %s to new frame" theme)
      (load-theme theme t))))

(add-hook 'after-make-frame-functions #'my/reapply-theme)

;; Better system font detection and settings
(defun antler/set-fonts ()
  "Set up fonts based on availability and system type."
  (let* ((font-families (font-family-list))
         (font-height (cond ((>= (display-pixel-width) 2560) 140)
                            ((>= (display-pixel-width) 1920) 120)
                            (t 110)))
         (preferred-fonts '("JetBrainsMono Nerd Font" "Fira Code" "Cascadia Code" "Menlo" "Monaco"))
         (available-font (seq-find (lambda (font) (member font font-families)) preferred-fonts)))
    
    (when available-font
      (set-face-attribute 'default nil :font available-font :height font-height)
      (set-face-attribute 'fixed-pitch nil :font available-font :height font-height)
      (set-face-attribute 'variable-pitch nil 
                          :font (or (car (member "Cantarell" font-families))
                                   (car (member "Segoe UI" font-families))
                                   available-font)
                          :height font-height))))

;; Set fonts after frames are created to handle both daemon and regular mode
(if (daemonp)
    (add-hook 'after-make-frame-functions
              (lambda (frame)
                (with-selected-frame frame (antler/set-fonts))))
  (antler/set-fonts))

;; Icon packages
(use-package all-the-icons
  :straight t
  :if (display-graphic-p))

(use-package all-the-icons-dired
  :straight t
  :after all-the-icons
  :hook (dired-mode . all-the-icons-dired-mode))

(use-package all-the-icons-completion
  :straight t
  :after (all-the-icons marginalia)
  :hook (marginalia-mode . all-the-icons-completion-marginalia-setup)
  :config
  (all-the-icons-completion-mode))

;; Modern child frame completion UI
(use-package kind-icon
  :straight t
  :after corfu
  :custom
  (kind-icon-default-face 'corfu-default)
  :config
  (add-to-list 'corfu-margin-formatters #'kind-icon-margin-formatter))

;; Highlight TODO and similar keywords in comments
(use-package hl-todo
  :straight t
  :hook (prog-mode . hl-todo-mode)
  :config
  (setq hl-todo-keyword-faces
        '(("TODO"  . "#FF0000")
          ("FIXME" . "#FF0000")
          ("NOTE"  . "#00FF00")
          ("HACK"  . "#9C91E4")
          ("DONE"  . "#44BC44")))
  (global-hl-todo-mode))

;; Ligature support for modern fonts
(use-package ligature
  :straight t
  :config
  ;; Enable ligatures in programming modes
  (ligature-set-ligatures 'prog-mode '("www" "**" "***" "**/" "*>" "*/" "\\\\" "\\\\\\" "{-" "::"
                                      ":::" ":=" "!!" "!=" "!==" "-}" "----" "-->" "->" "->>"
                                      "-<" "-<<" "-~" "#{" "#[" "##" "###" "####" "#(" "#?" "#_"
                                      "#_(" ".-" ".=" ".." "..<" "..." "?=" "??" ";;" "/*" "/**"
                                      "/=" "/>" "//" "///" "&&" "||" "||=" "|=" "|>" "^=" "$>" "++"
                                      "+++" "+>" "=:=" "==" "===" "==>" "=>" "=>>" "<=" "=<<" "=/="
                                      ">-" ">=" ">=>" ">>" ">>-" ">>=" ">>>" "<*" "<*>" "<|" "<|>"
                                      "<$" "<$>" "<!--" "<-" "<--" "<->" "<+" "<+>" "<=" "<==" "<=>"
                                      "<=<" "<>" "<<" "<<-" "<<=" "<<<" "<~" "<~~" "</" "</>" "~@"
                                      "~-" "~>" "~~" "~~>" "%%"))
  (global-ligature-mode t))

(provide 'ui-theme)
