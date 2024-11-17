;;; core-keybindings.el --- Global keybindings -*- lexical-binding: t -*-

;; Window management
(global-set-key (kbd "C-x |") 'split-window-right)
(global-set-key (kbd "C-x _") 'split-window-below)
(global-set-key (kbd "C-x }") 'enlarge-window-horizontally)
(global-set-key (kbd "C-x {") 'shrink-window-horizontally)
(global-set-key (kbd "C-x ^") 'enlarge-window)

;; Buffer management
(global-set-key (kbd "C-x C-b") 'ibuffer)
(global-set-key (kbd "C-x k") 'kill-this-buffer)

;; Navigation
(global-set-key (kbd "M-g g") 'avy-goto-line)
(global-set-key (kbd "C-;") 'avy-goto-char-timer)

;; Text manipulation
(global-set-key [remap dabbrev-expand] 'hippie-expand)
(global-set-key (kbd "C-c d") 'duplicate-line)
(global-set-key (kbd "M-j") 'join-line)

;; LSP keybindings
(with-eval-after-load 'lsp-mode
  (define-key lsp-mode-map (kbd "C-c l") lsp-command-map)
  (define-key lsp-mode-map (kbd "C-c l f") 'lsp-format-buffer)
  (define-key lsp-mode-map (kbd "C-c l r") 'lsp-rename)
  (define-key lsp-mode-map (kbd "C-c l a") 'lsp-execute-code-action))


(defun antler/kill-back-to-indentation ()
  "Kill from point back to the first non-whitespace character on the line."
  (interactive)
  (let ((prev-pos (point)))
    (back-to-indentation)
    (kill-region (point) prev-pos)))

(global-set-key (kbd "C-M-<backspace>") 'antler/kill-back-to-indentation)

;; Random line sorting
(defun sanityinc/sort-lines-random (beg end)
  "Sort lines in region from BEG to END randomly."
  (interactive "r")
  (save-excursion
    (save-restriction
      (narrow-to-region beg end)
      (goto-char (point-min))
      (let ;; To make `end-of-line' and etc. to ignore fields.
          ((inhibit-field-text-motion t))
        (sort-subr nil 'forward-line 'end-of-line nil nil
                   (lambda (s1 s2) (eq (random 2) 0)))))))

(defun antler/sort-lines-alphabetically (beg end)
  "Sort lines in region from BEG to END alphabetically."
  (interactive "r")
  (save-excursion
    (save-restriction
      (narrow-to-region beg end)
      (goto-char (point-min))
      (let ((inhibit-field-text-motion t))  ; To make `end-of-line' and etc. to ignore fields.
        (sort-subr nil 'forward-line 'end-of-line nil nil
                   (lambda (s1 s2) (string< (buffer-substring (car s1) (cdr s1))
                                       (buffer-substring (car s2) (cdr s2)))))))))

(global-set-key (kbd "C-M-r") 'sanityinc/sort-lines-random)
(global-set-key (kbd "C-M-a") 'antler/sort-lines-alphabetically)
(global-set-key (kbd "C-x C-b") 'ibuffer)
(global-set-key (kbd "M-/") 'hippie-expand)

(provide 'core-keybindings)
