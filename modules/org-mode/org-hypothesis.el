;;; org-hypothesis.el --- Org mode configuration -*- lexical-binding: t -*-

;; Define custom faces for different hypothesis states
(defface org-hypothesis-positive-face
  '((t :foreground "PaleGreen" :weight bold))
  "Face for positive (+) indicators in org mode")

(defface org-hypothesis-uncertain-face
  '((t :foreground "AntiqueWhite" :weight bold))
  "Face for uncertain (?) indicators in org mode")

(defface org-hypothesis-negative-face
  '((t :foreground "DimGray" :weight bold))
  "Face for negative (-) indicators in org mode")

(defface org-hypothesis-positive-text-face
  '((t :foreground "PaleGreen" :slant italic))
  "Face for text after positive (+) indicators")

(defface org-hypothesis-uncertain-text-face
  '((t :foreground "AntiqueWhite" :slant italic))
  "Face for text after uncertain (?) indicators")

(defface org-hypothesis-negative-text-face
  '((t :foreground "DimGray" :strike-through t))
  "Face for text after negative (-) indicators")

(defvar-local org-hypothesis-overlays nil
  "List of overlays for hypothesis indicators and text.")

(defun org-hypothesis-clear-overlays ()
  "Clear all hypothesis indicator overlays."
  (while org-hypothesis-overlays
    (delete-overlay (pop org-hypothesis-overlays))))

(defun org-hypothesis-highlight-indicators ()
  "Highlight hypothesis indicators using overlays."
  (org-hypothesis-clear-overlays)
  (save-excursion
    (goto-char (point-min))
    (while (re-search-forward "\\((\\([+?-]\\))\\)\\(.*\\)" nil t)
      (let* ((indicator (match-string 2))
             (indicator-start (match-beginning 1))
             (indicator-end (match-end 1))
             (text-start (match-end 1))
             (text-end (line-end-position))
             (indicator-overlay (make-overlay indicator-start indicator-end))
             (text-overlay (make-overlay text-start text-end)))
        (push indicator-overlay org-hypothesis-overlays)
        (push text-overlay org-hypothesis-overlays)
        ;; Set faces for indicator
        (overlay-put indicator-overlay 'face
                    (cond
                     ((string= indicator "+") 'org-hypothesis-positive-face)
                     ((string= indicator "?") 'org-hypothesis-uncertain-face)
                     ((string= indicator "-") 'org-hypothesis-negative-face)))
        ;; Set faces for following text
        (overlay-put text-overlay 'face
                    (cond
                     ((string= indicator "+") 'org-hypothesis-positive-text-face)
                     ((string= indicator "?") 'org-hypothesis-uncertain-text-face)
                     ((string= indicator "-") 'org-hypothesis-negative-text-face)))))))

(defun org-hypothesis-mode ()
  "Setup hypothesis highlighting for the current buffer."
  (org-hypothesis-highlight-indicators)
  (add-hook 'after-change-functions
            (lambda (&rest _) (org-hypothesis-highlight-indicators))
            nil t))

;; Refresh function
(defun org-hypothesis-refresh ()
  "Refresh hypothesis highlighting in current buffer"
  (interactive)
  (org-hypothesis-highlight-indicators))

(provide 'org-hypothesis)
