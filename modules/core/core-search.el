;;; tools-search.el --- Search tools configuration -*- lexical-binding: t -*-

(use-package ag
  :straight t
  :defer t
  :custom
  (ag-highlight-search t)
  (ag-reuse-window t)
  (ag-reuse-buffers t)
  :config
  (add-to-list 'ag-arguments "--hidden"))

(provide 'core-search)
