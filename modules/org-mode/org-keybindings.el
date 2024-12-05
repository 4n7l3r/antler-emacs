(use-package org
  :bind (:map org-mode-map
              ;; Undo tab override
              ("<tab>" . org-cycle)
              ;; Custom keybindings
              ("C-c C-S-l" . +org/remove-link)
              ("C-c C-i" . org-toggle-inline-images)
              ("S-RET" . +org/shift-return)
              ("C-RET" . +org/insert-item-below)
              ("C-S-RET" . +org/insert-item-above)
              ("C-M-RET" . org-insert-subheading)
              ("<C-return>" . +org/insert-item-below)
              ("<C-S-return>" . +org/insert-item-above)
              ("<C-M-return>" . org-insert-subheading)
              ([remap doom/backward-to-bol-or-indent] . org-beginning-of-line)
              ([remap doom/forward-to-last-non-comment-or-eol] . org-end-of-line)
              ;; macOS-specific bindings
              :prefix ("<s-return>" . macos)
              ([s-return] . +org/insert-item-below)
              ([s-S-return] . +org/insert-item-above)
              ([s-M-return] . org-insert-subheading)
              :prefix ("C-c" . "localleader")
              ;; Leader key bindings
              ("#" . org-update-statistics-cookies)
              ("'" . org-edit-special)
              ("*" . org-ctrl-c-star)
              ("+" . org-ctrl-c-minus)
              ("," . org-switchb)
              ("." . org-goto)
              ("@" . org-cite-insert)
              ;; Sub-prefixes
              :prefix ("a" . "attachments")
              ("a" . org-attach)
              ("d" . org-attach-delete-one)
              ("D" . org-attach-delete-all)
              ("f" . +org/find-file-in-attachments)
              ;; Continue with similar structure for the remaining keybindings...
              ))

(use-package org-agenda
  :after org
  :bind (:map org-agenda-mode-map
              ("C-SPC" . org-agenda-show-and-scroll-up)
              :prefix ("<leader>" . "localleader")
              ;; Leader key bindings
              :prefix ("d" . "date/deadline")
              ("d" . org-agenda-deadline)
              ("s" . org-agenda-schedule)
              :prefix ("c" . "clock")
              ("c" . org-agenda-clock-cancel)
              ("g" . org-agenda-clock-goto)
              ;; Continue with similar structure for the remaining keybindings...
              )))
