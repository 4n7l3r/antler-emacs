;;; core-keybindings.el --- Global keybindings -*- lexical-binding: t -*-

;; Require bind-key for consistent key binding
(require 'bind-key)

;; Window management packaged together
(use-package window
  :ensure nil  ; Built-in package
  :bind (("C-x |" . split-window-right)
         ("C-x _" . split-window-below)
         ("C-x }" . enlarge-window-horizontally)
         ("C-x {" . shrink-window-horizontally)
         ("C-x ^" . enlarge-window)))

;; Text manipulation functions packaged together
(use-package text-manipulation
  :ensure nil  ; This is an internal package
  :bind (("C-c d" . duplicate-line)
         ("M-j" . join-line)
         ("C-M-<backspace>" . antler/kill-back-to-indentation)
         ("C-M-r" . sanityinc/sort-lines-random)
         ("C-M-a" . antler/sort-lines-alphabetically))
  :config
  (defun antler/kill-back-to-indentation ()
    "Kill from point back to the first non-whitespace character on the line."
    (interactive)
    (let ((prev-pos (point)))
      (back-to-indentation)
      (kill-region (point) prev-pos)))

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
                                         (buffer-substring (car s2) (cdr s2))))))))))

(provide 'core-keybindings)
